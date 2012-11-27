module Graphics.Canvas.Util
    ( unsafeThawArr
    , unsafeFreezeArr
    )

where

import Control.Monad
import Control.Monad.Primitive

import Data.Array.Repa as R hiding ((++))

import qualified Data.Vector.Unboxed as VU


-- | Convert an immutable Repa array to a mutable vector.
unsafeThawArr :: (PrimMonad m, VU.Unbox e) =>
                 Array U sh e
              -> m (VU.MVector (PrimState m) e)
unsafeThawArr = VU.unsafeThaw . toUnboxed


-- | Convert a mutable vector to an immutable Repa array.
unsafeFreezeArr :: (PrimMonad m, Shape sh, VU.Unbox e) =>
                   sh
                -> VU.MVector (PrimState m) e
                -> m (Array U sh e)
unsafeFreezeArr sh = liftM (fromUnboxed sh) . VU.unsafeFreeze
