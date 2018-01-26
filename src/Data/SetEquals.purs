module Data.SetEquals where

import Prelude

import Data.SetPreserving (class SetPreserving)
import Data.SetPreserving as SP
import Type.Data.Boolean (False)
import Type.Equality (class TypeEqualsBool)

class SetEquals g h where
  to :: g -> h
  from :: h -> g

instance setEqualsReflexive :: SetEquals g g where
  to = id
  from = id
else instance setEqualsTransitive
  :: ( TypeEqualsBool g h False
     , TypeEqualsBool h k False
     , SetEquals g h
     , SetEquals h k
     ) => SetEquals g k
  where
        to = to <<< (to :: g -> h)
        from = from <<< (from :: k -> h)

instance setEqualsSetPreserving :: SetPreserving t => SetEquals g (t g) where
  to = SP.to
  from = SP.from

{-
  -- replace with transitive
instance setEqualsPreservingTransitive
  :: (SetPreserving t, SetEquals g h)
  => SetEquals (t g) (t h) where
  to = SP.to <<< to <<< SP.from
  from = SP.to <<< from <<< SP.from
-}

{-
instance setEqualsAdditive :: SetEquals (Additive g) g
  where
  to = unwrap
  from = wrap
-}

{-
instance setEqualsAdditive
  :: SetEquals g h
  => SetEquals (Additive g) (Additive h)
  where
  to = defaultTo
  from = wrap <<< from <<< (unwrap :: Additive h -> h)
-}

{-
defaultTo ::
  forall supernewtype supert subnewtype subt
  . Newtype supernewtype supert
 => Newtype subnewtype subt
 => SetEquals subt supert
 => subnewtype -> supernewtype
defaultTo = over wrap to
-}

{-
instance setEqualsTransitive
  :: (SetEquals g h, SetEquals h k)
  => SetEquals g k
  where
  to = (to :: h -> k) <<< to
  from = (from :: h -> g) <<< from
-}

--defaultTo :: forall h g. SetEquals g h => g -> h
--defaultTo = unsafeFromSuper

--defaultFrom :: forall h g. SetEquals g h => h -> g
--defaultFrom = toSuper
