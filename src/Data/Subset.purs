module Data.Subset where

import Prelude

import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe, fromJust)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Type.Data.Boolean (False)
import Type.Equality (class TypeEqualsBool)
import Type.Proxy (Proxy)

-- strict subsets for now
class TypeEqualsBool h g False <= Subset h g where
  toSuper :: h -> g
  fromSuper :: g -> Maybe h

instance subsetNumberInt :: Subset Int Number where
  toSuper = toNumber
  fromSuper = fromNumber

{-
  TODO: move this to non-strict subsets by incorporating set equality
instance subsetReflexive :: Subset g g where
  toSuper = id
  fromSuper = Just
-}

{-
TODO: move this somewhere else to make it safe?
instance subsetTransitive
  :: (TypeEqualsBool k g False, Subset h g, Subset k h)
  => Subset k g
  where
  toSuper = (toSuper :: h -> g) <<< toSuper
  fromSuper = (fromSuper :: h -> Maybe k) <=< fromSuper
-}

instance subsetAdditive
  :: (TypeEqualsBool (Additive h) (Additive g) False, Subset h g)
  => Subset (Additive h) (Additive g)
  where
  toSuper = defaultToSuper
  fromSuper = defaultFromSuper

instance subsetDual
  :: (TypeEqualsBool (Dual h) (Dual g) False, Subset h g)
  => Subset (Dual h) (Dual g)
  where
  toSuper = defaultToSuper
  fromSuper = defaultFromSuper

instance subsetMultiplicative
  :: (TypeEqualsBool (Multiplicative h) (Multiplicative g) False, Subset h g)
  => Subset (Multiplicative h) (Multiplicative g)
  where
  toSuper = defaultToSuper
  fromSuper = defaultFromSuper

toSuperSuper :: forall h g k. Subset h g => Subset k h => k -> g
toSuperSuper = toSuper <<< (toSuper :: k -> h)

fromSuperSuper :: forall h g k . Subset h g => Subset k h => g -> Maybe k
fromSuperSuper = fromSuper <=< (fromSuper :: g -> Maybe h)

defaultToSuper ::
  forall supernewtype supert subnewtype subt
  . Newtype supernewtype supert
 => Newtype subnewtype subt
 => Subset subt supert
 => subnewtype -> supernewtype
defaultToSuper = over wrap toSuper

defaultFromSuper ::
  forall supernewtype supert subnewtype subt
  . Newtype supernewtype supert
 => Newtype subnewtype subt
 => Subset subt supert
 => supernewtype -> Maybe subnewtype
defaultFromSuper = map wrap <<< (fromSuper :: supert -> Maybe subt) <<< unwrap

unsafeFromSuper :: forall h g. Subset h g => g -> h
unsafeFromSuper = unsafePartial fromJust <<< fromSuper

defaultAdd :: forall h g. Subset h g => Semiring g => Proxy g -> h -> h -> h
defaultAdd _ x y = (unsafeFromSuper :: g -> h) $ toSuper x + toSuper y

defaultZero :: forall h g. Subset h g => Semiring g => Proxy g -> h
defaultZero _ = (unsafeFromSuper :: g -> h) zero

defaultMul :: forall h g. Subset h g => Semiring g => Proxy g -> h -> h -> h
defaultMul _ x y = (unsafeFromSuper :: g -> h) $ toSuper x * toSuper y

defaultOne :: forall h g. Subset h g => Semiring g => Proxy g -> h
defaultOne _ = (unsafeFromSuper :: g -> h) one

defaultAppend :: forall h g
   . Subset h g
  => Semigroup g
  => Proxy g -> h -> h -> h
defaultAppend _ x y = (unsafeFromSuper :: g -> h) $ toSuper x <> toSuper y
