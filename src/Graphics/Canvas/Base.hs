{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|

Base types used by everyone else.

-}

module Graphics.Canvas.Base
    ( Point
    , origin
    , Pixel(..)
    , PixelData
    , PixelMask
    , Canvas(..)
    )

where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VG

import Data.Word


-- | Coordinates on a canvas. Components are row and column indices,
-- bottom-left canvas point is the 'origin'.
type Point = DIM2


origin :: Point
origin = (Z :. 0 :. 0)


-- | State of a single pixel.
newtype Pixel = RGBPixel (Word8, Word8, Word8) deriving Show


type PixelData = Array U DIM2 Pixel


type PixelMask = Array U DIM2 Bool


-- | Canvas is a rectangular region holding pixel data.
newtype Canvas = Canvas PixelData deriving Show


deriving instance (VG.Vector VU.Vector Pixel)
deriving instance (VG.MVector VU.MVector Pixel)
deriving instance (VU.Unbox Pixel)
deriving instance (Elt Pixel)
