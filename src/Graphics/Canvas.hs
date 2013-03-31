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
    , ellipticBrush
    , pixel
    , line
    , unsafeApply

    , Pixel(..)
    )

where

import Control.Monad
import Control.Monad.ST

import Data.Array.Repa as R hiding ((++))

import Graphics.Canvas.Base
import Graphics.Canvas.BBox
import Graphics.Canvas.Tools


-- | Create a blank canvas.
makeCanvas :: Int
           -- ^ Height
           -> Int
           -- ^ Width.
           -> Pixel
           -- ^ Initial state for every pixel of canvas.
           -> Canvas
makeCanvas height width initial =
    runST $ liftM Canvas $ 
    R.computeUnboxedP $
    R.fromFunction (R.ix2 height width) (\_ -> initial)


-- | Get a subregion of a canvas, if canvas dimensions match the
-- provided bounding box.
getRegion :: BBox
          -> Canvas
          -- ^ Source canvas.
          -> Maybe Canvas
getRegion bbox c@(Canvas arr) =
    case clampBBox bbox c of
      Nothing -> Nothing
      Just (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) ->
          let
              newArr = R.fromFunction 
                       (R.ix2 (y1 - y0 + 1) (x1 - x0 + 1)) $
                       \(Z :. y :. x) -> arr ! R.ix2 (y + y0) (x + x0)
          in runST $
             liftM (Just . Canvas) (R.computeUnboxedP newArr)
