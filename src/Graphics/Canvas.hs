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

import qualified Data.Vector.Unboxed as VU

import Graphics.Canvas.Base
import Graphics.Canvas.Tools


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
