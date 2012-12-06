{-# LANGUAGE PatternGuards #-}

{-|

All about bounding boxes.

-}

module Graphics.Canvas.BBox
    ( BBox(..)
    , within

    , clampBBox
    , wholeBBox
    , joinBBox
    , intersectBBox
    , overlaps
    )

where

import Data.Maybe

import Data.Array.Repa as R hiding ((++))

import Graphics.Canvas.Base


-- | A bounding box on a canvas, given by bottom-left and top-right
-- points. Edges of the box are filled (the set is closed).
newtype BBox = BBox (Point, Point) deriving Show


-- | Bounding box membership predicate.
within :: Point -> BBox -> Bool
within (Z :. py :. px) (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) =
    py >= y0 && py <= y1 &&
    px >= x0 && px <= x1


-- | Clamp bounding box limits to fit in canvas.
clampBBox :: BBox -> Canvas -> Maybe BBox
clampBBox bbox canvas =
    intersectBBox bbox (wholeBBox canvas)


-- | Bounding box matching a canvas extent.
wholeBBox :: Canvas -> BBox
wholeBBox (Canvas arr) =
    BBox (R.ix2 0 0, R.ix2 (y - 1) (x - 1))
    where
      (Z :. y :. x) = extent arr


-- | Join two bounding boxes, producing a bounding box covering every
-- point which belongs to at least one of the original boxes.
--
-- > +-------+           +-----------+
-- > |       |           |           |
-- > |   +---+---+       |           |
-- > |   |   |   |  ->   |           |
-- > +---+---+   |       |           |
-- >     |       |       |           |
-- >     +-------+       +-----------+
-- >
-- >  +--+                +----------+
-- >  |  |                |          |
-- >  +--+                |          |
-- >                ->    |          |
-- >   +---------+        |          |
-- >   |         |        |          |
-- >   +---------+        +----------+
joinBBox :: BBox -> BBox -> BBox
joinBBox (BBox (Z :. y0 :. x0, Z :. y1 :. x1))
         (BBox (Z :. y0' :. x0', Z :. y1' :. x1')) =
    let
        (y0'', x0'') = (min y0 y0', min x0 x0')
        (y1'', x1'') = (max y1 y1', max x1 x1')
    in
      BBox (R.ix2 y0'' x0'', R.ix2 y1'' x1'')


-- | Intersect two bounding boxes, producing a bounding box covering
-- every point which belongs to both of the original boxes. If there's
-- no intersection, return 'Nothing'.
--
-- > +-------+
-- > |       |
-- > |   +---+---+       +---+
-- > |   |   |   |  ->   |   |
-- > +---+---+   |       +---+
-- >     |       |
-- >     +-------+
-- >
-- > +---+
-- > |   |
-- > +---+
-- >                ->   Nothing
-- >   +---------+
-- >   |         |
-- >   +---------+
intersectBBox :: BBox -> BBox -> Maybe BBox
intersectBBox (BBox (Z :. y0 :. x0, Z :. y1 :. x1))
              (BBox (Z :. y0' :. x0', Z :. y1' :. x1'))
    | x0' > x1, x1' < x0, y0' > y1, y1' < y0 =
        let
            (y0'', x0'') = (min y0 y0', min x0 x0')
            (y1'', x1'') = (max y1 y1', max x1 x1')
        in
          Just $ BBox (R.ix2 y0'' x0'', R.ix2 y1'' x1'')
    | otherwise = Nothing


-- | Check if two boxes overlap.
overlaps :: BBox -> BBox -> Bool
overlaps b1 b2 = isJust (intersectBBox b1 b2)
