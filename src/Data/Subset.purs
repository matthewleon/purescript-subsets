module Data.Subset where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Data.Monoid.Additive (Additive(Additive))
import Data.Newtype (unwrap, over)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

class Subset g h where
  toSuper :: h -> g
  fromSuper :: g -> Maybe h

instance subsetAdditive :: Subset g h => Subset (Additive g) (Additive h)
  where
  toSuper = over Additive toSuper
  fromSuper = map Additive <<< fromSuper <<< unwrap

toSuperSuper :: forall g h k. Subset g h => Subset h k => k -> g
toSuperSuper = toSuper <<< (toSuper :: k -> h)

fromSuperSuper :: forall g h k . Subset g h => Subset h k => g -> Maybe k
fromSuperSuper = fromSuper <=< (fromSuper :: g -> Maybe h)

unsafeFromSuper :: forall g h. Subset g h => g -> h
unsafeFromSuper = unsafePartial fromJust <<< fromSuper

defaultAdd :: forall g h . Subset g h => Semiring g => Proxy g -> h -> h -> h
defaultAdd _ x y = (unsafeFromSuper :: g -> h) $ toSuper x + toSuper y

defaultZero :: forall g h . Subset g h => Semiring g => Proxy g -> h
defaultZero _ = (unsafeFromSuper :: g -> h) zero

defaultMul :: forall g h . Subset g h => Semiring g => Proxy g -> h -> h -> h
defaultMul _ x y = (unsafeFromSuper :: g -> h) $ toSuper x * toSuper y

defaultOne :: forall g h . Subset g h => Semiring g => Proxy g -> h
defaultOne _ = (unsafeFromSuper :: g -> h) one

defaultAppend :: forall g h
   . Subset g h
  => Semigroup g
  => Proxy g -> h -> h -> h
defaultAppend _ x y = (unsafeFromSuper :: g -> h) $ toSuper x <> toSuper y
