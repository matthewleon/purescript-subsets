module Test.Data.Subset where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Subset (class Subset, defaultAdd, defaultZero, defaultMul, defaultOne)
import Type.Proxy (Proxy(Proxy))

newtype NonZeroInt = NonZeroInt Int

instance subsetIntNonZeroInt :: Subset Int NonZeroInt where
  toSuper (NonZeroInt x) = x
  fromSuper x = if x >= 0 then Just (NonZeroInt x) else Nothing

instance semiringNonZeroInt :: Semiring NonZeroInt where
  add = defaultAdd (Proxy :: Proxy Int)
  zero = defaultZero (Proxy :: Proxy Int)
  mul = defaultMul (Proxy :: Proxy Int)
  one = defaultOne (Proxy :: Proxy Int)
