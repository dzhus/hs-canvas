{-|

All about bounding boxes.

-}

module Graphics.Canvas.BBox
    ( BBox(..)
    , ClampedBBox(..)

    , clampBBox
    , wholeBBox
    )

where

import Data.Array.Repa as R hiding ((++))

import Graphics.Canvas.Base


-- | Bounding box on a canvas, given by bottom-left and top-right
-- points. Edges of the box are filled.
newtype BBox = BBox (Point, Point) deriving Show


-- | Bounding box fully covered by the associated canvas.
--
-- Use the constructor directly with care and prefer 'clampBBox'
-- instead.
newtype ClampedBBox = ClampedBBox BBox deriving Show


-- | Clamp bounding box limits to fit in canvas.
clampBBox :: BBox -> Canvas -> ClampedBBox
clampBBox (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) (Canvas arr) =
    let
        (y0', x0') = (0, 0)
        (Z :. y1' :. x1') = extent arr
        (y0'', x0'') = (max y0 y0', max x0 x0')
        (y1'', x1'') = (min y1 (y1' - 1), min x1 (x1' - 1))
    in
      ClampedBBox $ BBox (R.ix2 y0'' x0'', R.ix2 y1'' x1'')


-- | Bounding box which of a whole canvas.
wholeBBox :: Canvas -> ClampedBBox
wholeBBox (Canvas arr) = ClampedBBox $ BBox (R.ix2 0 0, extent arr)
