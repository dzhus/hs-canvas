{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-|

Rasterization algorithms.

-}

module Graphics.Algorithms.Rasterization
    ( bresenham
    )

where

import qualified Data.Vector.Generic as V


-- | Bersenham's line algorithm.
--
-- Produce a vector of points on two-dimensional raster which
-- approximate a segment between two given points. Points in vector
-- are ordered from start to end.
--
-- Simple point type is used to accomodate to non-canvas/non-Repa users.
bresenham :: V.Vector v (Int, Int) =>
             (Int, Int)
          -- ^ Start point.
          -> (Int, Int)
          -- ^ End point.
          -> v (Int, Int)
bresenham p0@(x0, y0) p1@(x1, y1)
  | slope > 1 = V.map pointFlip $ bresenham (pointFlip p0) (pointFlip p1)
  | otherwise =
      V.unfoldr
           (\(x, y, err) ->
                let
                    !newErr' = err + slope
                    !(newY, newErr) =
                        if newErr' >= 0.5
                        then (y + yStep, newErr' - 1.0)
                        else (y, newErr')
                in
                  if x == (x1 + xStep)
                  then Nothing
                  else Just ((x, y), (x + xStep, newY, newErr))
           ) (x0, y0, 0)
    where
      pointFlip (x, y) = (y, x)
      slope = abs $ (fromIntegral $ y1 - y0) / (fromIntegral $ x1 - x0)
      xStep = if x0 < x1 then 1 else -1
      yStep = if y0 < y1 then 1 else -1
