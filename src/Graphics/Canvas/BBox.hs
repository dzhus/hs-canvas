{-|

All about bounding boxes.

-}

module Graphics.Canvas.BBox
    ( BBox(..)
    , ClampedBBox(..)

    , clampBBox
    , roundBBox
    )

where

import Data.Array.Repa as R hiding ((++))

import Graphics.Canvas.Base


-- | Bounding box on a canvas, given by top-left and bottom-right
-- points.
newtype BBox = BBox (Point, Point)


-- | Bounding box fully covered by the associated canvas.
newtype ClampedBBox = ClampedBBox BBox


-- | Bounding box for a round brush.
roundBBox :: Int
          -- ^ Radius
          -> Point -> BBox
roundBBox r (Z :. cy :. cx) =
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
