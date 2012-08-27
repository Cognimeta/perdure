{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleContexts, UndecidableInstances, TypeFamilies #-}

module Database.Perdure.SpaceBook(
  SpaceBook(..)
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Structured
import Control.Monad.State
import Database.Perdure.Persistent
import Database.Perdure.Space
import Database.Perdure.Count
import Database.Perdure.CSerializer(Address)
import Cgm.Data.Monoid
import Database.Perdure.ReplicatedFile
import Debug.Trace
import Cgm.Data.Super
import Database.Perdure.Package
import Cgm.Data.Typeable

moduleName :: String
moduleName = "Database.Perdure.SpaceBook"

data SpaceBook c s = SpaceBook {
  bookCount :: !(c Address), 
  bookSpace :: !s
  }

instance (Persistent (c Address), Persistent s) => Persistent (SpaceBook c s) where persister = structureMap persister
                                        
instance Typeable1 c => Typeable1 (SpaceBook c) where 
  typeOf1 _ = mkTyCon3 perdurePackage moduleName "SpaceBook" `mkTyConApp` [typeOf1 (undefined :: c ())]

deriveStructured ''SpaceBook
