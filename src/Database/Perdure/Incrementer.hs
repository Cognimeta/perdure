{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, Rank2Types, FlexibleContexts, ScopedTypeVariables, BangPatterns, GADTs #-}

module Database.Perdure.Incrementer (
  incr
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Structured
import Control.Monad.State
import Database.Perdure.Persistent
import Database.Perdure.Space.Space
import Database.Perdure.Count.Count
import Database.Perdure.CSerializer(Address)
import Cgm.Data.Monoid
import Database.Perdure.ReplicatedFile
import Cgm.Data.Multiset as MS
import Debug.Trace
import Database.Perdure.SpaceBook
import Database.Perdure.Ref

-- While Decrementer should be used on allocated values (not in s, and may have a count in c), Incrementer is used
-- on values which have already been written, but we use older values of s and c for which the value to increment
-- was not yet allocated. So most often it is s, and not in c.

incr :: (Multiset c, Space s) => Persister a -> a -> SpaceBook c s -> SpaceBook c s
incr !p !a !s = case p of
  PartialWordPersister n -> s
  PairPersister pb pc -> case a of (b, c) -> incr pc c $ incr pb b s
  EitherPersister pb pc -> either (incr pb) (incr pc) a s
  ViewPersister i pb -> incr pb (apply i a) s
  SummationPersister pi _ f -> f (\i pb _ b -> incr pb b $ incr pi i s) a
  DRefPersister' -> case a of (DRef _ _ warr) -> 
                                let referenced = incr persister $ deref a
                                in either (\(WordNArrayRef _ r _) -> incrRef r referenced) (\(WordNArrayRef _ r _) -> incrRef r referenced) (unwrap warr) s
  CRefPersister' _ pra -> onCRef (incr pra) (incr persister) a s

incrRef :: forall w s c. (LgMultiple Word64 w, Space s, Multiset c) =>
           BasicRef w -> (SpaceBook c s -> SpaceBook c s) -> SpaceBook c s -> SpaceBook c s
incrRef r children cs@(SpaceBook c s) =  let rs = refStart r in
  if isFreeSpace (getLen rs) s 
  then (\(SpaceBook c' s') -> SpaceBook c' $ removeSpan (refSpan r) s') $ children cs 
  else SpaceBook (MS.insert rs c) s

{-
newtype Incrementer c s a = Incrementer {getIncrementer :: (SpaceBook c s -> SpaceBook c s) :<- a} deriving Cofunctor
incr :: Bijection' (Incrementer c s a) (a -> SpaceBook c s -> SpaceBook c s)
incr = uncheckedBijection getRevFun RevFun . struct

-- apIncr and unIncr are workarounds for GHC 7.0.2 issue <http://hackage.haskell.org/trac/ghc/ticket/5002>
apIncr :: Incrementer c s a -> a -> SpaceBook c s -> SpaceBook c s
apIncr = apply incr
unIncr :: (a -> SpaceBook c s -> SpaceBook c s) -> Incrementer c s a
unIncr = retract incr

instance InjectionACofunctor (Incrementer c s) where iacomap = cofunctorIacomap
instance Multiset c => FixedPersister (Incrementer c s) where
  partialWordPersister n = retract incr $ const id
  (&.) = wrapB incr incr incr $ \ad bd (a, b) -> bd b . ad a
instance Multiset c => BoundedPersister (Incrementer c s) where
  (|.) = wrapB incr incr incr either
instance Multiset c => SeqPersister (Incrementer c s) where
  fromSeqPersister p = retract incr $ const id -- There are no references in seqPersisters
  summationPersister pi _ s = unIncr $ s (\i pb _ b -> apIncr pb b . apIncr pi i)
instance (Space s, Multiset c) => RPersister (Incrementer c s) where
  storeRefPersister = unIncr $ \r -> incrRef r id
  dRefPersister' p = unIncr $ \a@(DRef _ warr) -> 
    let referenced = apIncr p (deref a)
    in either (\(WordNArrayRef _ r _) -> incrRef r referenced) (\(WordNArrayRef _ r _) -> incrRef r referenced) $ unwrap warr
instance (Space s, Multiset c) => Persister (Incrementer c s) where
  cRefPersister' = (persister1 |. persister) >$< onCRef Left Right

deriveStructured ''Incrementer
-}
