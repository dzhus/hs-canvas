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
      !b2 = ellipticBrush 40 0.99 (RGBPixel (0, 255, 255))
      !p = pixel (RGBPixel (0, 0, 0))
      !l = line b
      !l2 = line b2
      !lp = line p
      x = (BBox ((Z :. 400 :. 400), (Z :. 520 :. 520)))
  unsafeApply l ((Z :. 0 :. 0), (Z :. 9999 :. 9999)) c
  unsafeApply l2 ((Z :. 0 :. 9999), (Z :. 9999 :. 0)) c
  unsafeApply lp ((Z :. 5000 :. 0), (Z :. 5000 :. 9999)) c
  unsafeApply lp ((Z :. 0 :. 5000), (Z :. 9999 :. 5000)) c
  runIL $ writeImage fileName (toImage c)
