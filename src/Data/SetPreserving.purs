module Data.SetPreserving where

import Data.Monoid.Additive (Additive)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (unwrap, wrap)

class SetPreserving t where
  to :: forall a. a -> t a
  from :: forall a. t a -> a

instance setPreservingAdditive :: SetPreserving Additive where
  to = wrap
  from = unwrap

instance setPreservingMultiplicative :: SetPreserving Multiplicative where
  to = wrap
  from = unwrap

instance setPreservingDual :: SetPreserving Dual where
  to = wrap
  from = unwrap
