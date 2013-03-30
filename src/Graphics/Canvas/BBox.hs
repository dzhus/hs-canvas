{-# LANGUAGE PatternGuards #-}

{-|

All about bounding boxes.

-}

module Graphics.Canvas.BBox
    ( BBox(..)
    , within


    -- * Bounding box combinators
    , clampBBox
    , wholeBBox
    , overlaps
    -- ** Repositioning & central points
    , centerBBoxTo
    , centerPoint
    -- ** Set operations
    , joinBBox
    , intersectBBox

    -- * Repa interface
    , bboxExtent
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


-- | Reposition a bounding box @bb@ so that its center is in a point
-- @p@. The following holds:
--
-- > centerPoint (centerBBoxTo bb p) = p
centerBBoxTo :: BBox -> Point -> BBox
centerBBoxTo (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) (Z :. cy :. cx) =
    BBox (R.ix2 (cy - yShift - yR) (cx - xShift - xR),
          R.ix2 (cy + yShift) (cx + xShift))
    where
      -- Carry 1 to remainder for proper snapping
      width = x1 - x0
      (xShift, xR) = width `divMod` 2
      height = y1 - y0
      (yShift, yR) = height `divMod` 2


-- | Return center point of a bounding box. If a dimension of the box
-- is even, the corresponding coordinate of the center point is
-- rounded up.
centerPoint :: BBox -> Point
centerPoint (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) = 
    R.ix2 (y0 + y + yR) (x0 + x + xR)
    where
      (x, xR) = (x1 - x0) `divMod` 2
      (y, yR) = (y1 - y0) `divMod` 2


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
    | x0' > x1 || x1' < x0 || y0' > y1 || y1' < y0 = Nothing
    | otherwise = Just $ BBox (R.ix2 y0'' x0'', R.ix2 y1'' x1'')
                  where
                    (y0'', x0'') = (max y0 y0', max x0 x0')
                    (y1'', x1'') = (min y1 y1', min x1 x1')


-- | Check if two boxes overlap.
overlaps :: BBox -> BBox -> Bool
overlaps b1 b2 = isJust (intersectBBox b1 b2)


bboxExtent :: BBox -> DIM2
bboxExtent (BBox (Z :. y0 :. x0, Z :. y1 :. x1)) =
    R.ix2 (y1 - y0 + 1) (x1 - x0 + 1)
