{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|

Tools to draw on a canvases and primitives to define your own tools.

-}

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
    , nib
    , squareMaskTool
    , line
    , polygon

    -- * Using tools
    , unsafeApply
    )

where

import Data.Array.Repa as R hiding ((++), map)
import Data.Array.Repa.Eval

import Data.Maybe

import qualified Data.Vector as V (Vector)
import qualified Data.Vector.Unboxed as VU (elem)
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


-- | Predicate for points within an ellipse given by the focal points.
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
-- canvas at the point where the tool was applied (the click point).
brushOperation :: PixelData
               -- ^ Pixels to be copied onto a canvas at the click
               -- point. Must have odd dimensions. Copying is
               -- performed so that the central point of this array
               -- matches the click point.
               -> PixelMask
               -- ^ Transparency data. Extent must match that of the
               -- pixel data argument. If a value in this array is
               -- True, then the corresponding pixel from pixel data
               -- array is copied onto the canvas. Otherwise, the
               -- original pixel value is preserved.
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
                    fillBlock2S (VGM.unsafeWrite mvec)
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


-- | Produce a constant color tool with a square mask of given
-- dimensions using the provided mask predicate.
squareMaskTool :: Int
               -- ^ Mask dimension, must be odd.
               -> Pixel
               -- ^ Color.
               -> (Point -> Bool)
               -- ^ A predicate used to build 'PixelMask' within the
               -- square.
               -> Tool SP
squareMaskTool dim value maskPred
    | odd dim = Tool $ brushOperation pixelData pixelMask
    | otherwise = error "Mask dimension must be odd"
    where
      ex = R.ix2 dim dim
      pixelData = fromUnboxed ex $ VG.replicate (size ex) value
      pixelMask = computeUnboxedS $ fromFunction ex maskPred


roundBrush :: Int
           -- ^ Radius.
           -> Pixel
           -- ^ Color.
           -> Tool SP
roundBrush radius value = squareMaskTool dim value maskPred
    where
      dim = 2 * radius + 1
      center = R.ix2 radius radius
      maskPred = circlePredicate center radius


ellipticBrush :: Int
              -- ^ Semi-major axis.
              -> Double
              -- ^ Eccentricity (between @0.0@ and @1.0@).
              -> Double
              -- ^ Brush angle in radians (between @0.0@ and @pi@).
              -- When this angle is zero, major axis of the ellipse
              -- will be aligned with positive direction of the
              -- horizontal axis.
              -> Pixel
              -- ^ Color.
              -> Tool SP
ellipticBrush a ecc phi value = squareMaskTool dim value maskPred
    where
      dim = 2 * a + 1
      a' = fromIntegral a
      -- Focal distance
      fr = fromIntegral (round $ ecc * a' :: Int)
      xShift = round $ fr * cos phi
      yShift = round $ fr * sin phi
      f1 = R.ix2 (a - yShift) (a - xShift)
      f2 = R.ix2 (a + yShift) (a + xShift)
      maskPred = ellipsePredicate f1 f2 (2 * a)


-- | Italic nib.
nib :: Int
    -- ^ Nib stroke width.
    -> Int
    -- ^ Nib thickness.
    -> Double
    -- ^ Nib angle in radians (between @0.0@ and @pi@), used to
    -- produce oblique nibs. When this angle is zero, nib is held
    -- horizontally.
    -> Pixel
    -- ^ Color.
    -> Tool SP
nib w t phi value = Tool $ brushOperation pixelData pixelMask
    where
      w' = fromIntegral w
      py = round $ w' * sin phi
      px = round $ w' * cos phi
      ex = R.ix2 (max 1 $ abs py) (max 1 $ abs px)
      pixelData = fromUnboxed ex $ VG.replicate (size ex) value
      !nibPoints = bresenham (0, 0) (py, px)
      pixelMask = computeUnboxedS $ fromFunction ex $
                  \(Z :. y :. x) -> VU.elem (y, x) nibPoints


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


-- | Class of tool input arities for which an @n@-ary input may be
-- tranformed (exploded) into a vector of @(n-1)@-ary inputs.
class Explode higher lower where
    reduce :: higher -> V.Vector lower


instance Explode DP SP where
    reduce (Z :. p1y :. p1x, Z :. p2y :. p2x) =
        VG.map (\(y, x) -> R.ix2 y x) $
        bresenham (p1y, p1x) (p2y, p2x)


instance Explode [SP] DP where
    reduce pts@(_:ptail) = VG.zip (VG.fromList pts) (VG.fromList ptail)
    reduce [] = VG.empty


-- | Raise arity of a tool.
raise :: (Explode b a) => Tool a -> Tool b
raise (Tool act') = Tool $
    \input canvas ->
        let
            rInputs = reduce input
            bcs :: V.Vector (BBox, Commit)
            bcs = VG.map fromJust $ VG.filter isJust $
                  VG.map (\i -> act' i canvas) rInputs
        in
          if VG.null bcs
          then Nothing
          else
              let
                  -- Cumulative commit (applied sequentially)
                  commit = \canvas' -> VG.mapM_ (\(_, a) -> a canvas') bcs
                  -- Cumulative bounding box
                  bbox = VG.foldl1 joinBBox $ VG.map fst bcs
              in
                Just (bbox, commit)


-- | Apply a tool along a line given by two endpoints.
line :: Tool SP -> Tool DP
line = raise


-- | Apply a tool along lines of a polygon given by vertices, in that
-- order.
polygon :: Tool DP -> Tool [SP]
polygon = raise


-- | Apply a tool to a canvas without checking of bounds and locks.
unsafeApply :: (Tool input) -> input -> Canvas -> IO BBox
unsafeApply (Tool op) input canvas =
    case op input canvas of
      Just (bbox, commit) -> commit canvas >> return bbox
