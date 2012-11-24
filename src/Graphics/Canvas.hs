{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}

{-|

Canvas types and functions.

-}

module Graphics.Canvas
    ( Canvas
    , makeCanvas

    , Tool(..)
    , roundBrush
    )

where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VG

import Data.Word
import GHC.Base

import Graphics.Canvas.Base
import Graphics.Canvas.BBox
import Graphics.Canvas.Util


-- | Create a blank canvas
makeCanvas :: Int
           -- ^ Height
           -> Int
           -- ^ Width.
           -> Pixel
           -- ^ Initial state for every pixel of canvas.
           -> Canvas
makeCanvas height width initial =
    Canvas $ R.fromUnboxed (R.ix2 height width) $
    VU.replicate (height * width) initial


-- | Unsafe operation which applies the tool at a point of a canvas.
type ToolOperation = Point -> Canvas -> IO (Canvas, ClampedBBox)


-- | An operation which can be applied to canvas.
data Tool = Tool { apply :: ToolOperation
                 }


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
          newArr <- unsafeFreezeArr fullShape mvec
          return (Canvas newArr, bb)


roundBrush :: Int -> Pixel -> Tool
roundBrush !radius !value = Tool $ brushOperation (roundBBox radius) f
    where
      f !targetArr !clickPoint !testPoint =
          if roundPredicate clickPoint radius testPoint
          then value
          else targetArr ! testPoint
