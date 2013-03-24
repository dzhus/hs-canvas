{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL

import System.Environment

import Graphics.Canvas
import Graphics.Canvas.DevIL

main = do
  (fileName:_) <- getArgs
  let !c = makeCanvas 10000 10000 (RGBPixel (255, 255, 255))
      !b = roundBrush 40 (RGBPixel (255, 0, 255))
      !p = pixel (RGBPixel (255, 0, 255))
      !l = line p
      x = (BBox ((Z :. 400 :. 400), (Z :. 520 :. 520)))
  unsafeApply l ((Z :. 0 :. 0), (Z :. 9900 :. 9999)) c
  runIL $ writeImage fileName (toImage c)
