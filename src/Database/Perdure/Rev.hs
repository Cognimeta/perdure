{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, RankNTypes, EmptyDataDecls, DeriveDataTypeable, TypeOperators, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Database.Perdure.Rev(
  (:>)(..),
  onRev,
  NoRev,
  onNoRev,
  toCurrent,
  toOnlyRev,
  revPersister,
  latestLens
  ) where

import Database.Perdure.Persistent
import Database.Perdure.Ref
import Cgm.Data.Tagged
import Data.Lens
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

class Rev a where lastRev :: Tagged a Integer
instance Rev NoRev where lastRev = tag (-1)
instance Rev b => Rev (a :> b) where lastRev = tag $ (at :: At b) lastRev +  1

class Rev a => PersistentRev a where
  deserRev :: (forall b. Persister b -> (b -> a) -> z) -> Integer -> z
  serRev :: (forall b. Integer -> Persister b -> (b -> a) -> b -> z) -> a -> z
instance PersistentRev NoRev where  
  deserRev _ _ = error "bad index when deserializing Rev"
  serRev f a = onNoRev a
instance (Persistent b, PersistentRev r) => PersistentRev (b :> r) where
  deserRev f i = if i == (at :: At (b :> r)) lastRev then f persister Current else deserRev ((. (Previous .)) . f) i
  serRev f = onRev (f ((at :: At (b :> r)) lastRev) persister Current) (serRev $ ((. (Previous .)) .) . f)
  
-- | The persister for '(:>)' first writes out the numeric index, from the right, in the chain of revisions. This way the chain of alternative
-- revisions can lengthen without changing the indices of past revisions.
revPersister :: PersistentRev a => Persister a
revPersister = summationPersister persister deserRev serRev

instance Persistent NoRev where persister = revPersister
instance PersistentRev (b :> r) => Persistent (b :> r) where persister = revPersister                                

latestLens :: (b -> a) -> Lens (a :> b) a
latestLens toLatest = lens (toCurrent toLatest) (const . Current)

-- Easy to implement, a bit more general than (&.), but does not belong in FixedPersister. Does not seem to help up with coproducts.
--Persister a -> (a -> Persister b) -> Persister (a, b)
