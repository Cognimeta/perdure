{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, Rank2Types, FlexibleContexts, ScopedTypeVariables, BangPatterns, GADTs #-}

module Database.Perdure.Decrementer (
  decr
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Structured
import Database.Perdure.Persistent
import Database.Perdure.Space
import Cgm.Data.Multiset as MS
import Database.Perdure.SpaceBook
import Database.Perdure.Deref
import qualified Database.Perdure.Cache as Cache
import Database.Perdure.ArrayRef
import Control.Concurrent.MVar
import System.IO.Unsafe

-- TODO consider addding a (lazy) bool into persisters that says if StoreRefs are present in descendents, and checking it to prune parts of the traversal

-- | Has effects through unsafePerformIO on the caches stored in the DRefs (removes any cache entries for the deallocated allocations).
decr :: Persister a -> a -> SpaceBook -> SpaceBook
decr !p !a !s = case p of
  PartialWordPersister _ -> s
  PairPersister pb pc -> case a of (b, c) -> decr pc c $ decr pb b s
  EitherPersister pb pc -> either (decr pb) (decr pc) a s
  ViewPersister i pb -> decr pb (apply i a) s
  SummationPersister pi' _ f -> f (\i pb _ b -> decr pb b $ decr pi' i s) a
  DRefPersister' -> case a of (DRef _ (DeserializerContext _ cache) warr) -> 
                                let referenced = decr persister $ let da = deref a in da `seq` unsafeClearCache cache (arrayRefAddr warr) `seq` da
                                in either (\(WordNArrayRef _ r _) -> decrRef r referenced) (\(WordNArrayRef _ r _) -> decrRef r referenced) (unwrap warr) s
  CRefPersister' _ pra -> onCRef (decr pra) (decr persister) a s

{-# NOINLINE unsafeClearCache #-}
unsafeClearCache :: MVar Cache -> Len Word64 Word64 -> ()
unsafeClearCache c addr = unsafePerformIO $ modifyMVar_  c $ return . Cache.delete addr

decrRef :: forall w. LgMultiple Word64 w => 
           BasicRef w -> (SpaceBook -> SpaceBook) -> SpaceBook -> SpaceBook
decrRef r onDealloc sb@(SpaceBook c s) =
  maybe ((\(SpaceBook c' s') -> SpaceBook c' ({-trace ("freeing span " ++ show (refSpan r)) $-} addSpan (refSpan r) s')) $ onDealloc sb)
  (flip SpaceBook s) $ MS.delete (refStart r) c
