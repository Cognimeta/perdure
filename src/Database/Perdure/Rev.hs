{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, RankNTypes, EmptyDataDecls, DeriveDataTypeable, TypeOperators, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, TemplateHaskell #-}

module Database.Perdure.Rev(
  (:>)(..),
  onRev,
  NoRev,
  onNoRev,
  toCurrent,
  toOnlyRev,
  revPersister,
  latestLens,
  Rev,
  Revs,
  HasPrev(..),
  revLens,
  unrev,
  rev
  ) where

import Prelude()
import Cgm.Prelude
import Database.Perdure.Persistent
import Control.Lens hiding (At, at)
import Data.Typeable

infixr 1 :>
-- | A data type which is equivalent to Either, but which is persisted in an open manner which allows us to chain new
-- variants on the left. As more variants are added (going from NoRev to V1 :> NoRev and then to V2 :> V1 :> NoRev), the
-- persisted representation gets expanded without breaking the representation of previous variants.
-- We do not use Either because of the risk of persisting it in the standard manner and therefore losing upgradability.
data a :> b = Current a | Previous b deriving Typeable

onRev :: (a -> z) -> (b -> z) -> (a :> b) -> z
onRev az bz r = case r of {Current a -> az a; Previous b -> bz b}

-- | An uninhabited type used as the last (rightmost) type in chains of '(:>)'
data NoRev deriving Typeable
onNoRev :: NoRev -> z
onNoRev _ = undefined

toOnlyRev :: (a :> NoRev) -> a
toOnlyRev = toCurrent onNoRev

-- | Converts a chain of revisions to the 'Current' type, given a way to convert the 'Previous' type to the 'Current' type.
toCurrent :: (b -> a) -> (a :> b) -> a
toCurrent = onRev id

class LastRev a where lastRev :: Tagged a Integer
instance LastRev NoRev where lastRev = tag (-1)
instance LastRev b => LastRev (a :> b) where lastRev = tag $ (at :: At b) lastRev +  1

class LastRev a => PersistentRev a where
  deserRev :: (forall b. Persister b -> (b -> a) -> z) -> Integer -> z
  serRev :: (forall b. Integer -> Persister b -> (b -> a) -> b -> z) -> a -> z
instance PersistentRev NoRev where  
  deserRev _ _ = error "bad index when deserializing Rev"
  serRev _ = onNoRev
instance (Persistent b, PersistentRev r) => PersistentRev (b :> r) where
  deserRev f i = if i == (at :: At (b :> r)) lastRev then f persister Current else deserRev ((. (Previous .)) . f) i
  serRev f = onRev (f ((at :: At (b :> r)) lastRev) persister Current) (serRev $ ((. (Previous .)) .) . f)
  
-- | The persister for '(:>)' first writes out the numeric index, from the right, in the chain of revisions. This way the chain of alternative
-- revisions can lengthen without changing the indices of past revisions.
revPersister :: PersistentRev a => Persister a
revPersister = summationPersister persister deserRev serRev

instance Persistent NoRev where persister = revPersister
instance PersistentRev (b :> r) => Persistent (b :> r) where persister = revPersister                                

-- | This is not a legal lens since it violates the law which says that setting back what you got must have no effect.
-- Here it is almost true since the only effect it has is to upgrade to the current representation, an idempotent change
-- for a semantically equivalent value.                                                             
latestLens :: (b -> a) -> Lens' (a :> b) a
latestLens toLatest = lens (toCurrent toLatest) (const Current)

-------------------------------------
-- Below we intoduce a typeclass. It simplifies usage, but only works when types have unique predecessors

class HasPrev a where
  type Prev a
  fromPrev :: Prev a -> a

type Revs a = a :> Prev a

newtype Rev a = Rev (Revs a) deriving Typeable
deriveStructured ''Rev
instance (HasPrev a, PersistentRev (a :> Prev a)) => Persistent (Rev a) where persister = structureMap revPersister

revLens :: forall a. HasPrev a => Lens' (Rev a) a
revLens = iso (\(Rev a)->a) Rev . latestLens (fromPrev :: Prev a -> a)

unrev :: HasPrev a => Rev a -> a
unrev (Rev a) = toCurrent fromPrev a

rev :: a -> Rev a
rev = Rev . Current
