{-|

Tools to draw on a canvases and primitives to define your own tools.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Graphics.Canvas.Tools
    ( Tool(..)
    , Action
    , Commit
    , SP
    , DP

    -- * Predefined tools
    , roundBrush
    , ellipticBrush
    , pixel
    , line

    -- * Using tools
    , unsafeApply
    )

where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval

import Data.Maybe

import qualified Data.Vector as V (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import GHC.Base

import Graphics.Algorithms.Rasterization
import Graphics.Canvas.Base
import Graphics.Canvas.BBox
import Graphics.Canvas.Util


-- | An unsafe operation which destructively modifies a portion of a
-- canvas.
type Commit = Canvas -> IO ()


-- | An action may be applied to a canvas to obtain a bounding box and
-- a 'Commit', failing if the action was generated with an
-- out-of-bounds input. The produced bounding box defines a region of
-- the canvas affected by the action.
--
-- Commit must be then applied to the canvas it was generated for.
-- However, all tools are written in such a way that a commit may be
-- unsafely extracted from an action and applied to a different
-- canvas.
type Action = Canvas -> Maybe (BBox, Commit)


-- | A tool with certain parameters which can be applied to canvas.
--
-- Tool is parametrized by an input type it expects to produce an
-- action. Think clicks as inputs.
newtype Tool a = Tool (a -> Action)


-- | Input arity=1.
type SP = Point


-- | Input arity=2.
type DP = (Point, Point)


-- | 1x1 bounding box of a point.
pointBBox :: Point -> BBox
pointBBox p = BBox (p, p)


-- | Predicate for points within the specified distance from a central
-- point.
circlePredicate :: Point
               -- ^ Central point.
               -> Int
               -- ^ Distance.
               -> Point
               -- ^ Test point.
               -> Bool
circlePredicate c r pt = distance c pt <= fromIntegral r


ellipsePredicate :: Point
                 -- ^ First focal point.
                 -> Point
                 -- ^ Second focal point.
                 -> Int
                 -- ^ Maximum sum of distances from a test point to
                 -- foci (major axis of the ellipse).
                 -> Point
                 -- ^ A test point.
                 -> Bool
ellipsePredicate f1 f2 r pt =
    distance f1 pt + distance f2 pt <= fromIntegral r


-- | Make a tool which copies data from 'PixelData' to a portion of a
-- canvas within a bounding box which depends on the point where the
-- tool was applied (the click point).
brushOperation :: PixelData
               -- ^ Pixels to be copied onto a canvas at the click
               -- point. Must have odd dimensions.
               -> PixelMask
               -- ^ Transparency data. Extent must match that of the
               -- pixel data argument.
               -> (SP -> Action)
brushOperation !pixelData !pixelMask =
    \clickPoint@(Z :. yc :. xc) canvas ->
        case clampBBox (centerBBoxTo baseBBox clickPoint) canvas of
          Nothing -> Nothing
          Just bb@(BBox ((Z :. (I# y0) :. (I# x0)), _)) ->
            let
                -- Convert click point coordinates to
                commit :: Commit
                commit = \(Canvas arr') ->
                  let
                      -- Calculate offsets of a point relative to
                      -- the click point and use them to pick an
                      -- element from source arrays.
                      sourceFunction pt@(Z :. yt :. xt) =
                          if pixelMask ! p'
                          then pixelData ! p'
                          else arr' ! pt
                          where
                            p' = (Z :. y' :. x')
                            y' = yb + yt - yc
                            x' = xb + xt - xc
                      {-# INLINE sourceFunction #-}
                      !fullShape@(Z :. _ :. (I# width)) = extent arr'
                      !(Z :. (I# h0) :. (I# w0)) = bboxExtent bb
                  in do
                    mvec <- unsafeThawArr arr'
                    fillBlock2P (VGM.unsafeWrite mvec)
                                sourceFunction width x0 y0 w0 h0
                    _ <- unsafeFreezeArr fullShape mvec
                    return ()
            in
              Just (bb, commit)
       where
         (Z :. yBuf :. xBuf) = extent pixelData
         baseBBox = BBox (origin, Z :. yBuf - 1 :. xBuf - 1)
         -- Central point of pixel data & mask arrays
         (Z :. yb :. xb) = centerPoint baseBBox



roundBrush :: Int
           -- ^ Radius.
           -> Pixel
           -- ^ Color.
           -> Tool SP
roundBrush radius value = Tool $ brushOperation pixelData pixelMask
    where
      dim = 2 * radius + 1
      ex = R.ix2 dim dim
      center = R.ix2 radius radius
      pixelData = fromUnboxed ex $ VG.replicate (size ex) value
      pixelMask = computeUnboxedS $
                  fromFunction ex (circlePredicate center radius)


ellipticBrush :: Int
              -- ^ Semi-major axis.
              -> Double
              -- ^ Eccentricity (between @0.0@ and @1.0@).
              -> Pixel
              -- ^ Color.
              -> Tool SP
ellipticBrush a ecc value = Tool $ brushOperation pixelData pixelMask
    where
      dim = 2 * a + 1
      ex = R.ix2 dim dim
      -- Focal distance
      fr = round $ ecc * fromIntegral a
      f1 = R.ix2 a (a - fr)
      f2 = R.ix2 a (a + fr)
      pixelData = fromUnboxed ex $ VG.replicate (size ex) value
      pixelMask = computeUnboxedS $
                  fromFunction ex (ellipsePredicate f1 f2 (2 * a))


pixel :: Pixel
      -- ^ Color.
      -> Tool SP
pixel value = Tool $
    \clickPoint c ->
        let
            bb = pointBBox clickPoint
            commit :: Commit
            commit = \(Canvas arr') -> do
              let fullShape = extent arr'
              let n = toIndex fullShape clickPoint
              mvec <- unsafeThawArr arr'
              VGM.unsafeWrite mvec n value
              _ <- unsafeFreezeArr fullShape mvec
              return ()
        in
          intersectBBox bb (wholeBBox c) >> return (bb, commit)


-- | Apply a tool along the line.
line :: Tool SP -> Tool DP
line (Tool act') = Tool $
    \(Z :. p1y :. p1x, Z :. p2y :. p2x) canvas ->
        let
            points = bresenham (p1x, p1y) (p2x, p2y)

            -- BBoxes & commits in every point of the line.
            bcs :: V.Vector (BBox, Commit)
            bcs = VG.map fromJust $ VG.filter isJust $
                  VG.map (\(x, y) -> act' (R.ix2 x y) canvas) points
        in
          if VG.null bcs
          then Nothing
          else
              let
                  -- Cumulative commit
                  commit = \canvas' ->
                           VG.mapM_ (\(_, a) -> a canvas') bcs

                  -- Cumulative bounding box
                  bbox = VG.foldl1 joinBBox $ VG.map fst bcs
              in
                Just (bbox, commit)


-- | Apply a tool to a canvas without checking of bounds and locks.
unsafeApply :: (Tool input) -> input -> Canvas -> IO BBox
unsafeApply (Tool op) input canvas =
    case op input canvas of
      Just (bbox, commit) -> commit canvas >> return bbox
