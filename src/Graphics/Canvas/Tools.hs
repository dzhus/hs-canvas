{-|

Tools to draw on a canvases and primitives to define your own tools.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Graphics.Canvas.Tools
    ( Tool(..)
    , Action
    , Change
    , SP
    , DP

    -- * Predefined tools
    , roundBrush
    , pixel

    -- * Using tools
    , unsafeApply

    -- * Tool helpers
    , brushOperation
    , roundPredicate
    )

where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval

import qualified Data.Vector.Generic.Mutable as VG

import GHC.Base

import Graphics.Canvas.Base
import Graphics.Canvas.BBox
import Graphics.Canvas.Util


-- | Unsafe operation which changes a portion of a canvas. Original
-- canvas is modified.
type Change = Canvas -> IO ()


-- | When applying a tool to a canvas, we either obtain a bounding box
-- and the operation, or fail in case action was generated with
-- out-of-bounds input.
type Action = Canvas -> Maybe (BBox, Change)


-- | Input arity=1.
type SP = Point


-- | Input arity=2.
type DP = (Point, Point)


-- | A tool with certain parameters which can be applied to canvas.
data Tool a = Tool (a -> Action)


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
roundPredicate !(Z :. cy :. cx) !r !(Z :. y :. x) =
    sqrt (fromIntegral ((x - cx) ^ 2 + (y - cy) ^ 2)) <= fromIntegral r


-- | Change a portion of a canvas within a bounding box which depends
-- on the point where the tool was applied (the click point).
brushOperation :: (Point -> BBox)
              -- ^ Compute the bounding box of operation given the
              -- click point.
              -> (PixelData -> Point -> Point -> Pixel)
              -- ^ Compute new pixels within the bounding box. Called
              -- with the existing canvas data, the click point and an
              -- every point within the bounding box.
              -> (SP -> Action)
brushOperation bboxFunction newPixelFunction =
    \clickPoint c@(Canvas targetArr) ->
        case clampBBox (bboxFunction clickPoint) c of
          Just (bb@(BBox ((Z :. (I# y0) :. (I# x0)),
                          (Z :. (I# y1) :. (I# x1))))) ->
            Just (bb, change)
            where
              !fullShape@(Z :. _ :. (I# width)) = extent targetArr
              !w0 = x1 -# x0 +# 1#
              !h0 = y1 -# y0 +# 1#
              sourceFunction = newPixelFunction targetArr clickPoint

              change :: Change
              change = \(Canvas arr) -> do
                mvec <- unsafeThawArr arr
                fillBlock2P (VG.unsafeWrite mvec)
                            sourceFunction width x0 y0 w0 h0
                _ <- unsafeFreezeArr fullShape mvec
                return ()
          Nothing -> Nothing


roundBrush :: Int
           -- ^ Radius.
           -> Pixel
           -- ^ Color.
           -> Tool SP
roundBrush !radius !value = Tool $ brushOperation (roundBBox radius) f
    where
      f !targetArr !clickPoint !testPoint =
          if roundPredicate clickPoint radius testPoint
          then value
          else targetArr ! testPoint


pixel :: Pixel
      -- ^ Color.
      -> Tool SP
pixel !value = Tool $
    \clickPoint c@(Canvas targetArr) ->
        let
            bb = pointBBox clickPoint
            fullShape = extent targetArr
            n = toIndex fullShape clickPoint
            change :: Change
            change = \(Canvas arr) -> do
              mvec <- unsafeThawArr arr
              VG.unsafeWrite mvec n value
              _ <- unsafeFreezeArr fullShape mvec
              return ()
        in intersectBBox bb (wholeBBox c) >> return (bb, change)


-- | Apply a tool to a canvas without checking bounds/locks.
unsafeApply :: (Tool a) -> a -> Canvas -> IO ()
unsafeApply (Tool act') inp' canvas =
    case act' inp' canvas of
      Just (_, ch) -> ch canvas
