{-|

Tools to draw on a canvases and primitives to define your own tools.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}

module Graphics.Canvas.Tools
    ( Tool(..)
    , Action
    , P1A
    , P2A

    -- * Predefined tools
    , roundBrush
    , pixel

    -- * Tool helpers
    , brushOperation
    , roundPredicate
    )

where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval

import qualified Data.Vector.Generic.Mutable as VG

import GHC.Base

import Graphics.Canvas.Base
import Graphics.Canvas.BBox
import Graphics.Canvas.Util


-- | Unsafe operation which changes a portion of a canvas. Original
-- canvas is modified. Result is a region which has been changed.
type Action = Canvas -> IO BBox


type P1A = Point -> Action
type P2A = Point -> Point -> Action


-- | A tool with certain parameters which can be applied to canvas.
data Tool a where
    -- We encode arity information in the type. We can match on arity
    -- using one of the specific types of Tool. This still feels
    -- awkward and newtypey.
    PointTool    :: P1A -> Tool P1A
    TwoPointTool :: P2A -> Tool P2A


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


pointBBox :: Point -> BBox
pointBBox p = BBox (p, p)


-- | Predicate for points within the specified distance from a central
-- point.
roundPredicate :: Point
               -- ^ Central point.
               -> Int
               -- ^ Distance.
               -> Point
               -- ^ Test point.
               -> Bool
roundPredicate !(Z :. cy :. cx) !r !(Z :. y :. x) =
    sqrt (fromIntegral ((x - cx) ^ 2 + (y - cy) ^ 2)) <= fromIntegral r


-- | Change a portion of a canvas within a bounding box which depends
-- on the point where the tool was applied (the click point).
brushOperation :: (Point -> BBox)
              -- ^ Compute the bounding box of operation given the
              -- click point.
              -> (PixelData -> Point -> Point -> Pixel)
              -- ^ Compute new pixels within the bounding box. Called
              -- with the existing canvas data, the click point and a
              -- point within the bounding box.
              -> P1A
brushOperation bboxFunction newPixelFunction =
    \(!clickPoint) (!c@(Canvas targetArr)) ->
        let
            !fullShape@(Z :. _ :. (I# width)) = extent targetArr
            sourceFunction = newPixelFunction targetArr clickPoint
        in
          case clampBBox (bboxFunction clickPoint) c of
            (Just
             (bb@(BBox ((Z :. (I# y0) :. (I# x0)),
                        (Z :. (I# y1) :. (I# x1)))))) ->
             do
              mvec <- unsafeThawArr targetArr
              fillBlock2P (VG.unsafeWrite mvec)
                          sourceFunction width x0 y0 w0 h0
              _ <- unsafeFreezeArr fullShape mvec
              return bb
              where
                !w0 = x1 -# x0 +# 1#
                !h0 = y1 -# y0 +# 1#
            Nothing -> error "Out of canvas bounds"


roundBrush :: Int
           -- ^ Radius.
           -> Pixel
           -- ^ Color.
           -> Tool P1A
roundBrush !radius !value = PointTool $ brushOperation (roundBBox radius) f
    where
      f !targetArr !clickPoint !testPoint =
          if roundPredicate clickPoint radius testPoint
          then value
          else targetArr ! testPoint


pixel :: Pixel
      -- ^ Color.
      -> Tool P1A
pixel !value = PointTool $
    \clickPoint (Canvas targetArr) ->
        let
            fullShape = extent targetArr
            n = toIndex fullShape clickPoint
            bb = pointBBox clickPoint
        in do
          mvec <- unsafeThawArr targetArr
          VG.unsafeWrite mvec n value
          _ <- unsafeFreezeArr fullShape mvec
          return bb
