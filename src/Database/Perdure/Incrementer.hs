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

module Database.Perdure.Incrementer (
  incr
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Structured
import Cgm.Data.Word
import Cgm.Data.Len
import Database.Perdure.Persistent
import Database.Perdure.Space
import Cgm.Data.Multiset as MS
import Database.Perdure.SpaceBook
import Database.Perdure.Deref
import Database.Perdure.StoreFile

-- While Decrementer should be used on allocated values (not in s, and may have a count in c), Incrementer is used
-- on values which have already been written, but we use older values of s and c for which the value to increment
-- was not yet allocated. So most often it is s, and not in c.

incr :: Persister a -> a -> SpaceBook -> SpaceBook
incr !p !a !s = case p of
  PartialWordPersister _ -> s
  PairPersister pb pc -> case a of (b, c) -> incr pc c $ incr pb b s
  EitherPersister pb pc -> either (incr pb) (incr pc) a s
  ViewPersister i pb -> incr pb (apply i a) s
  SummationPersister pi' _ f -> f (\i pb _ b -> incr pb b $ incr pi' i s) a
  DRefPersister' -> case a of (DRef _ _ warr) -> 
                                let referenced = incr persister $ deref a
                                in either (\(WordNArrayRef _ r _) -> incrRef r referenced) (\(WordNArrayRef _ r _) -> incrRef r referenced) (unwrap warr) s
  CRefPersister' _ pra -> onCRef (incr pra) (incr persister) a s

incrRef :: forall w. LgMultiple Word64 w =>
           BasicRef w -> (SpaceBook -> SpaceBook) -> SpaceBook -> SpaceBook
incrRef r children cs@(SpaceBook c s) =  let rs = refStart r in
  if isFreeSpace (getLen rs) s 
  then (\(SpaceBook c' s') -> SpaceBook c' $ removeSpan (refSpan r) s') $ children cs 
  else SpaceBook (MS.insert rs c) s
