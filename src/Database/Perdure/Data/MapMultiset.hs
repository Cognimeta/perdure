{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}

module Database.Perdure.Data.MapMultiset(
  MapMultiset(mapMultisetMap),
  module Cgm.Data.Multiset
  ) where

import Prelude()
import Cgm.Prelude
import Cgm.Data.Maybe
import Cgm.Data.Multiset
import Database.Perdure.Persistent
import Cgm.Control.Monad.State
import Database.Perdure.Data.Map(Map)
import qualified Database.Perdure.Data.Map as Map
import Data.Dynamic
import Cgm.Data.Typeable
import Database.Perdure.Package

newtype MapMultiset a = MapMultiset {mapMultisetMap :: Map a Integer} deriving Typeable

instance Multiset MapMultiset where
  emptySet = MapMultiset Map.empty
  insert e (MapMultiset a) = MapMultiset $ Map.insertWith (+) e 1 a -- TODO implement and use insertWith' (strict application of combining function)
  delete e (MapMultiset a) = fmap MapMultiset $ snd $ flip runState a $ Map.updateM e $ 
                             get >>= maybe (return ()) (\c -> if c == 1 then put Nothing else put $ Just $ c - 1)

instance (Typeable a, Persistent a) => Persistent (MapMultiset a) where persister = structureMap persister
                                                                        
deriveStructured ''MapMultiset
  