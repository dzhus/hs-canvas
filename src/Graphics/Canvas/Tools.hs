{-|

Tools to draw on a canvases and primitives to define your own tools.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Graphics.Canvas.Tools
    ( Tool(..)
    , ToolOperation

    -- * Predefined tools
    , roundBrush

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


-- | Unsafe operation which applies the tool at a point of a canvas.
-- Original canvas is modified. Result is a region which changed.
type ToolOperation = Point -> Canvas -> IO ClampedBBox


-- | An operation which can be applied to canvas.
data Tool = Tool { apply :: ToolOperation
                 }


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


-- | Predicate for points within specified distance from central
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
              -- with the existing canvas data, the click point and a
              -- point within the bounding box.
              -> ToolOperation
brushOperation bboxFunction newPixelFunction =
    \(!clickPoint) (!c@(Canvas targetArr)) ->
        let
            !fullShape@(Z :. _ :. (I# width)) = extent targetArr

            -- Calculate bounds for fillBlock2P
            !bb@(ClampedBBox
                 (BBox ((Z :. (I# y0) :. (I# x0)),
                        (Z :. (I# y1) :. (I# x1))))) =
                 clampBBox (bboxFunction clickPoint) c

            !w0 = x1 -# x0 +# 1#
            !h0 = y1 -# y0 +# 1#

            sourceFunction = newPixelFunction targetArr clickPoint
        in do
          mvec <- unsafeThawArr targetArr
          fillBlock2P (VG.unsafeWrite mvec)
                      sourceFunction width x0 y0 w0 h0
          _ <- unsafeFreezeArr fullShape mvec
          return bb


roundBrush :: Int -> Pixel -> Tool
roundBrush !radius !value = Tool $ brushOperation (roundBBox radius) f
    where
      f !targetArr !clickPoint !testPoint =
          if roundPredicate clickPoint radius testPoint
          then value
          else targetArr ! testPoint
