{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|

Canvas types and functions.

-}

module Graphics.Canvas
    ( Canvas
    , makeCanvas

    , Tool(..)
    , brushTool
    )

where

import Prelude hiding (Maybe, Just, Nothing)

import Control.Monad
import Control.Monad.Primitive

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VG

import Data.Word
import GHC.Base


-- | Canvas is a rectangular region holding pixel data.
newtype Canvas = Canvas (Array U DIM2 Pixel)


-- | State of a single pixel.
newtype Pixel = Pixel (Word8, Word8, Word8)

deriving instance (VG.Vector VU.Vector Pixel)
deriving instance (VG.MVector VU.MVector Pixel)
deriving instance (VU.Unbox Pixel)
deriving instance (Elt Pixel)


-- | Coordinates on a canvas. Components are row and column indices,
-- bottom-left canvas point is the origin.
type Point = DIM2


-- | Bounding box on a canvas, given by top-left and bottom-right
-- points.
newtype BBox = BBox (Point, Point)


-- | Bounding box fully covered by the associated canvas.
newtype ClampedBBox = ClampedBBox BBox


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


brushTool :: Int -> Pixel -> Tool
brushTool radius value = Tool $ brushApply radius value


unsafeThawArr :: (PrimMonad m, VU.Unbox e) => 
                 Array U sh e 
              -> m (VU.MVector (PrimState m) e)
unsafeThawArr = VU.unsafeThaw . toUnboxed


unsafeFreezeArr :: (PrimMonad m, Shape sh, VU.Unbox e) => 
                   sh
                -> VU.MVector (PrimState m) e                 
                -> m (Array U sh e)
unsafeFreezeArr sh = liftM (fromUnboxed sh) . VU.unsafeFreeze


brushApply :: Int -> Pixel -> ToolOperation
brushApply radius value = \(!clickPoint) !(c@(Canvas targetArr)) ->
    let
        !fullShape@(Z :. _ :. (I# width)) = extent targetArr

        -- Calculate bounds for fillBlock2P
        !bb@(ClampedBBox
             (BBox ((Z :. (I# y0) :. (I# x0)),
                    (Z :. (I# y1) :. (I# x1))))) =
                    clampBBox (brushBBox radius clickPoint) c
        !w0 = x1 -# x0 +# 1#
        !h0 = y1 -# y0 +# 1#

        -- New value of pixel within the bounding box of brush event
        sourceFunction !testPoint =
            if roundPredicate clickPoint radius testPoint
            then value
            else targetArr ! testPoint
    in do
      mvec <- unsafeThawArr targetArr
      fillBlock2P (VG.unsafeWrite mvec)
                  sourceFunction width x0 y0 w0 h0
      newArr <- unsafeFreezeArr fullShape mvec
      return (Canvas newArr, bb)


-- | Bounding box for round brush at point
brushBBox :: Int -> Point -> BBox
brushBBox r (Z :. cy :. cx) =
    let
        tl = Z :. cy - r :. cx - r
        br = Z :. cy + r :. cx + r
    in
      BBox (tl, br)


-- | Clamp bounding box limits to fit in canvas.
clampBBox :: BBox -> Canvas -> ClampedBBox
clampBBox (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) (Canvas arr) =
    let
        (y0', x0') = (0, 0)
        (Z :. y1' :. x1') = extent arr
        (y0'', x0'') = (max y0 y0', max x0 x0')
        (y1'', x1'') = (min y1 (y1' - 1), min x1 (x1' - 1))
    in
      ClampedBBox $ BBox ((Z :. y0'' :. x0''), (Z :. y1'' :. x1''))
