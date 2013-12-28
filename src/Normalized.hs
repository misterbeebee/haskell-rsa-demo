{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
module Normalized(
    FunArg,
    -- todo: make `Normalize` available to libraries that need to override Show instance, but not to general consumers
    NormalizedBy(Normalize),
    newNormalizedBy, Normalizer, normalizedLike,
    unsafeValue,
    (<%>), omap2
    ) where

import           Control.Exception    (assert)
import           Data.MonoTraversable

-- We use Data.MonoTraversable's Element for two purposes:
-- (1) to indicate the type of value stored in a Normalizer
-- (2) to indicate the type of value stored in

-- `FunArg` is similar to `Data.MonoTraversable.Element`, but distinct, to avoid confusion.
-- `FunArg ma` is the base type of value stored in a Mod* type
-- Later on, we define `Element` instances relating the NormalizedBy values to those base values,
-- for use with `MonoFunctor`'s `omap`
type family FunArg ma
type instance FunArg (GeneralNormalizer a) = a

class (Eq a, Show a) => (Normalizer a) where
    (<%>) :: FunArg a -> a -> FunArg a



---------- Section: GeneralNormalizer
data GeneralNormalizer a = (Eq a) => General (a -> a) String  -- String must be a unique string representation

normalizerGeneral :: (Eq a) => (a->a) -> String -> GeneralNormalizer a
normalizerGeneral = General

instance Show (GeneralNormalizer a) where
  show (General f repr) = "% " ++ repr

instance (Eq a) => Normalizer (GeneralNormalizer a) where
    (<%>) v (General f _ ) = f v

instance (Eq a) => Eq (GeneralNormalizer a) where
  (General _ repr1) == (General _ repr2)  =  repr1 == repr2


-------- Section: NormalizedBy
data NormalizedBy ma = Normalize {
    normalizer  :: (Normalizer ma) => ma,  -- Rank2Types
    unsafeValue :: (Normalizer ma) => FunArg ma -- Rank2Types
}

deriving instance (Show a, Normalizer a, Show (FunArg a)) => Show (NormalizedBy a)
-- stanalone deriving is needed because we used the class constraint in the `data` declaration
-- FlexibleContexts for `Show (Element a)`
-- UndecidableInstances needed for "constraint is no smaller than head"

-- cleanly construct a NormalizedBy value, whose value is properly modulated (initialized)
newNormalizedBy normalizer preValue = omap id (Normalize normalizer preValue)

-- In case a value was not constructed using a smart constructor,
-- the safest way to read a value is with a smart accessor
value :: (Normalizer mma) => NormalizedBy mma -> FunArg mma
value x = unsafeValue $ omap id x -- unsafeValue is safe here, since omap applies modularizer
-- todo: consider adding a boolean to NormalizedBy to indicate that it has been safely modulated
-- and doesn't need to be re-checked

-- This "collapses" the double-wrap of NormalizedBy-wraps-Mod-wraps-element:
-- `NormalizedBy ModInteger` is a a wrapper for `Integer`,
-- and `NormalizedBy GeneralNormalizer a` is a a wrapper for `a`,
type instance Element (NormalizedBy ma) = FunArg ma


-- OK, all that setup work pays of here, where we define `MonoFunctor` instance
-- that automatically modulates the results of arithmetic function applications
instance MonoFunctor (NormalizedBy ma) where
    -- omap :: (FunArg a -> FunArg a) -> NormalizedBy a -> NormalizedBy a
    omap f (Normalize normalizer x) = Normalize normalizer $ f x <%> normalizer

-- Applicator-like thing
-- todo: express this in terms of a more general typeclass.
omap2 :: (Normalizer ma) => (FunArg ma -> FunArg ma-> FunArg ma) -> NormalizedBy ma -> NormalizedBy ma -> NormalizedBy ma
omap2 f x y =
    assert (normalizer x == normalizer y) $
        omap (f $ unsafeValue x) y


-- Creates a new NormalizedBy value, using the same normalizer as previous.
normalizedLike :: NormalizedBy ma -> FunArg ma -> NormalizedBy ma
normalizedLike old newValue = omap (const newValue) old


