{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, GADTs #-}

module Database.Perdure.RNF (
  prnf
  ) where

import Cgm.Data.WordN
import Database.Perdure.Persistent
--import Database.Perdure.SoftRef
--import Database.Perdure.WriteRef

prnf :: Persister a -> a -> ()
prnf p = case p of
  PartialWordPersister _ -> (`seq` ())
  PairPersister pa pb -> \(a, b) -> prnf pa a `seq` prnf pb b
  EitherPersister pa pb -> either (prnf pa) (prnf pb)
  ViewPersister i pb -> prnf pb . apply i
  SummationPersister pi' _ s -> s (\i pb _ b -> prnf pi' i `seq` prnf pb b)
  DRefPersister' -> (`seq` ())  -- Do not load, the persisted reference is already fully evaluated
  CRefPersister' _ pra -> onCRef (prnf pra) (prnf persister)
