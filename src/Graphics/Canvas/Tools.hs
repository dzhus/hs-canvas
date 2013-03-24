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
    , pixel
    , line

    -- * Using tools
    , unsafeApply

    -- * Tool helpers
    , brushOperation
    , roundPredicate
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
data Tool a = Tool (a -> Action)


-- | Input arity=1.
type SP = Point


-- | Input arity=2.
type DP = (Point, Point)


-- | Bounding box for a round brush.
roundBBox :: Int
          -- ^ Radius
          -> Point -> BBox
roundBBox r (Z :. cy :. cx) =
    let
        tl = Z :. cy - r :. cx - r
        br = Z :. cy + r :. cx + r
    in
      BBox (tl, br)


-- | 1x1 bounding box of a point.
pointBBox :: Point -> BBox
pointBBox p = BBox (p, p)


-- | Predicate for points within the specified distance from a central
-- point.
roundPredicate :: Point
               -- ^ Central point.
               -> Int
               -- ^ Distance.
               -> Point
               -- ^ Test point.
               -> Bool
roundPredicate (Z :. cy :. cx) r (Z :. y :. x) =
    sqrt (fromIntegral (rx * rx + ry * ry) :: Double) <= fromIntegral r
    where
      rx = x - cx
      ry = y - cy


-- | Make an action which changes a portion of a canvas within a
-- bounding box which depends on the point where the tool was applied
-- (the click point).
brushOperation :: (Point -> BBox)
               -- ^ Compute the bounding box of operation given the
               -- click point.
               -> (PixelData -> Point -> Point -> Pixel)
               -- ^ Compute new pixels within the bounding box. Called
               -- with the existing canvas data, the click point and an
               -- every point within the bounding box.
               -> (SP -> Action)
brushOperation bboxFunction newPixelFunction =
    \clickPoint c ->
        case clampBBox (bboxFunction clickPoint) c of
          Nothing -> Nothing
          Just (bb@(BBox ((Z :. (I# y0) :. (I# x0)),
                          (Z :. (I# y1) :. (I# x1))))) ->
            let
              commit :: Commit
              commit = \(Canvas arr') ->
                let
                    sourceFunction = newPixelFunction arr' clickPoint
                    !fullShape@(Z :. _ :. (I# width)) = extent arr'
                    !w0 = x1 -# x0 +# 1#
                    !h0 = y1 -# y0 +# 1#
                in do
                  mvec <- unsafeThawArr arr'
                  fillBlock2P (VGM.unsafeWrite mvec)
                              sourceFunction width x0 y0 w0 h0
                  _ <- unsafeFreezeArr fullShape mvec
                  return ()
            in
              Just (bb, commit)


roundBrush :: Int
           -- ^ Radius.
           -> Pixel
           -- ^ Color.
           -> Tool SP
roundBrush radius value = Tool $ brushOperation (roundBBox radius) f
    where
      f targetArr clickPoint testPoint =
          if roundPredicate clickPoint radius testPoint
          then value
          else targetArr ! testPoint


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
