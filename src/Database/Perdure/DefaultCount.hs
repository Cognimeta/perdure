{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Database.Perdure.DefaultCount(
  DefaultCount(..),
  defaultCount,
  latestCount
  ) where

import Cgm.Data.Lens
import Database.Perdure.Rev
import Data.Lens
import Data.Lens.Template
import Cgm.Data.Multiset
import Database.Perdure.Data.MapMultiset
import Data.Typeable

newtype DefaultCount a = DefaultCount {_defaultCount :: MapMultiset a <: NoRev} deriving Typeable
latestCount :: Lens (DefaultCount a) (MapMultiset a)
latestCount = defaultCount . latestLens onNoRev
instance Multiset DefaultCount where 
  emptySet = DefaultCount $ Current emptySet
  insert e = modL latestCount $ insert e
  delete e = modL latestCount $ delete e

makeLenses [''DefaultCount]
