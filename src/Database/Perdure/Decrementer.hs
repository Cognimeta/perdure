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

module Database.Perdure.Decrementer (
  decr
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
import Cgm.Data.Multiset as MS
import Database.Perdure.SpaceBook
import Database.Perdure.Ref
import qualified Data.Cache.LRU as LRU
import Data.Dynamic
import Database.Perdure.ArrayRef
import Control.Concurrent.MVar
import System.IO.Unsafe
import Debug.Trace

-- TODO consider addding a (lazy) bool into persisters that says if StoreRefs are present in descendents, and checking it to prune parts of the traversal

-- | Has effects through unsafePerformIO on the caches stored in the DRefs (removes any cache entries for the deallocated allocations).
decr :: (Multiset c, Space s) => Persister a -> a -> SpaceBook c s -> SpaceBook c s
decr !p !a !s = case p of
  PartialWordPersister n -> s
  PairPersister pb pc -> case a of (b, c) -> decr pc c $ decr pb b s
  EitherPersister pb pc -> either (decr pb) (decr pc) a s
  ViewPersister i pb -> decr pb (apply i a) s
  SummationPersister pi _ f -> f (\i pb _ b -> decr pb b $ decr pi i s) a
  DRefPersister' -> case a of (DRef _ (DeserializerContext _ cache) warr) -> 
                                let referenced = decr persister $ let da = deref a in da `seq` (unsafeClearCache cache $ arrayRefAddr warr) `seq` da
                                in either (\(WordNArrayRef _ r _) -> decrRef r referenced) (\(WordNArrayRef _ r _) -> decrRef r referenced) (unwrap warr) s
  CRefPersister' _ pra -> onCRef (decr pra) (decr persister) a s

{-# NOINLINE unsafeClearCache #-}
unsafeClearCache :: MVar Cache -> Len Word64 Word64 -> ()
unsafeClearCache c addr = unsafePerformIO $ modifyMVar_  c $ return . fst . LRU.delete addr

decrRef :: forall w s c. (LgMultiple Word64 w, Space s, Multiset c) => 
           BasicRef w -> (SpaceBook c s -> SpaceBook c s) -> SpaceBook c s -> SpaceBook c s
decrRef r onDealloc sb@(SpaceBook c s) =
  maybe ((\(SpaceBook c s') -> SpaceBook c (trace ("freeing span " ++ show (refSpan r)) $ addSpan (refSpan r) s')) $ onDealloc sb) 
  (flip SpaceBook s) $ MS.delete (refStart r) c

{-

-- TODO Change type so it mappend is not potentially quadratic. Doing as in Incrementer would do the trick
newtype Decrementer c a = Decrementer {getDecrementer :: State (c Address) [Span] :<- a} deriving Cofunctor
decr :: Bijection' (Decrementer c a) (a -> State (c Address) [Span])
decr = uncheckedBijection getRevFun RevFun . struct

-- apDecr and unDecr are workarounds for GHC 7.0.2 issue <http://hackage.haskell.org/trac/ghc/ticket/5002>
apDecr :: Decrementer c a -> a -> State (c Address) [Span]
apDecr = apply decr
unDecr :: (a -> State (c Address) [Span]) -> Decrementer c a
unDecr = retract decr


instance InjectionACofunctor (Decrementer c) where iacomap = cofunctorIacomap
instance FixedPersister (Decrementer c) where
  partialWordPersister n = retract decr $ const $ return mempty
  (&.) = wrapB decr decr decr $ \ad bd (a, b) -> liftM2 mappend (ad a) (bd b)
instance BoundedPersister (Decrementer c) where
  (|.) = wrapB decr decr decr either
instance SeqPersister (Decrementer c) where
  fromSeqPersister p = retract decr $ const $ return mempty -- There are no references in seqPersisters
  summationPersister pi _ s = unDecr $ s (\i pb _ b -> liftM2 mappend (apDecr pi i) (apDecr pb b))
instance Multiset c => RPersister (Decrementer c) where
  storeRefPersister = unDecr $ \r -> decrRef r $ return mempty -- Non-DRef StoreRefs require no recursion on deallocation
  dRefPersister' p = unDecr $ \a@(DRef _ warr) -> 
    let onDealloc = apDecr p (deref a)
    in either (\(WordNArrayRef _ r _) -> decrRef r onDealloc) (\(WordNArrayRef _ r _) -> decrRef r onDealloc) $ unwrap warr
instance Multiset c => Persister (Decrementer c) where
  cRefPersister' = (persister1 |. persister) >$< onCRef Left Right


deriveStructured ''Decrementer

-}