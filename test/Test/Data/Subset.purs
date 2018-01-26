module Test.Data.Subset where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Subset (class Subset, defaultAdd, defaultZero, defaultMul, defaultOne)
import Type.Proxy (Proxy(Proxy))

newtype NonNegativeInt = NonNegativeInt Int

instance subsetNonNegativeIntInt :: Subset NonNegativeInt Int where
  toSuper (NonNegativeInt x) = x
  fromSuper x = if x >= 0 then Just (NonNegativeInt x) else Nothing

instance semiringNonNegativeInt :: Semiring NonNegativeInt where
  add = defaultAdd int
  zero = defaultZero int
  mul = defaultMul int
  one = defaultOne int

int :: Proxy Int
int = Proxy
