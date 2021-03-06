{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleContexts, UndecidableInstances, TypeFamilies, DeriveDataTypeable #-}

module Database.Perdure.SpaceBook(
  SpaceBook(..)
  ) where

import Prelude ()
import Cgm.Data.Structured
import Database.Perdure.Persistent
import Database.Perdure.SpaceTree
import Database.Perdure.Count
import Database.Perdure.Data.MapMultiset
import Cgm.Data.Typeable

data SpaceBook = SpaceBook {
  bookCount :: !(MapMultiset Address), 
  bookSpace :: !SpaceTree
  } deriving Typeable

instance Persistent SpaceBook where persister = structureMap persister
                                        
deriveStructured ''SpaceBook
