{-# LANGUAGE BangPatterns #-}

module Graphics.Canvas.DevIL
    ( toImage
    )

where

import Control.Monad
import Control.Monad.ST

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.IO.DevIL

import Graphics.Canvas.Base


-- | Convert canvas to a DevIL RGB image.
toImage :: Canvas -> Image
toImage (Canvas arr) =
    let
        (Z :. height :. width) = R.extent arr
        imgArr = R.fromFunction (Z :. height :. width :. 3) $
                 \(Z :. y :. x :. c) ->
                 let
                     RGBPixel (r, g, b) = arr ! (R.ix2 y x)
                 in
                   case c of
                     0 -> r
                     1 -> g
                     2 -> b
    in runST $
      liftM RGB $ R.computeP imgArr
