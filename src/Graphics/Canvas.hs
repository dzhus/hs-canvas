{-# LANGUAGE Rank2Types #-}

{-|

Canvas types and functions.

-}

module Graphics.Canvas
    ( -- * Canvas basics
      Canvas
    , makeCanvas

    -- ** Slicing canvas
    , BBox(..)
    , getRegion

    -- * Canvas operations
    , Tool(..)
    , roundBrush

    , Pixel(..)
    )

where

import Control.Monad
import Control.Monad.ST

import Data.Array.Repa as R hiding ((++))

import qualified Data.Vector.Unboxed as VU

import Graphics.Canvas.Base
import Graphics.Canvas.BBox
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


getRegion :: BBox
          -> Canvas
          -- ^ Source canvas.
          -> Canvas
getRegion bbox c@(Canvas arr) =
    let
        (ClampedBBox (BBox ((Z :. y0 :. x0), (Z :. y1 :. x1)))) =
            clampBBox bbox c
        newArr = R.fromFunction 
                 (R.ix2 (y1 - y0 + 1) (x1 - x0 + 1)) $
                 \(Z :. y :. x) -> arr ! (R.ix2 (y + y0) (x + x0))
    in runST $
      liftM Canvas $ R.computeUnboxedP newArr
