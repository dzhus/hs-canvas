{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL

import System.Environment

import Graphics.Canvas
import Graphics.Canvas.DevIL

main = do
  (fileName:_) <- getArgs
  let !c = makeCanvas 1000 1000 (RGBPixel (255, 255, 255))
      !b = roundBrush 40 (RGBPixel (255, 0, 255))
      !b2 = ellipticBrush 40 0.75 (2 * pi / 3) (RGBPixel (0, 255, 255))
      !p = pixel (RGBPixel (0, 0, 0))
      !l = line b
      !l2 = line b2
      !lp = line p
      !poly = polygon l2
      x = (BBox ((Z :. 400 :. 400), (Z :. 520 :. 520)))
  unsafeApply l ((Z :. 0 :. 0), (Z :. 999 :. 999)) c
  unsafeApply l2 ((Z :. 0 :. 999), (Z :. 999 :. 0)) c
  unsafeApply b (Z :. 500 :. 500) c
  unsafeApply b2 (Z :. 500 :. 500) c
  unsafeApply lp ((Z :. 0 :. 500), (Z :. 999 :. 500)) c
  unsafeApply lp ((Z :. 500 :. 0), (Z :. 500 :. 999)) c
  unsafeApply poly [ (Z :. 200 :. 200)
                   , (Z :. 100 :. 700)
                   , (Z :. 800 :. 800)
                   , (Z :. 700 :. 100)
                   ] c
  runIL $ writeImage fileName (toImage c)
