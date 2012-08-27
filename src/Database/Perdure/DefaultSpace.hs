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

module Database.Perdure.DefaultSpace(
  DefaultSpace(..),
  defaultSpace,
  latestSpace
  ) where

import Cgm.Data.Lens
import Database.Perdure.Rev
import Data.Lens
import Data.Lens.Template
import Database.Perdure.Space
import Database.Perdure.SpaceTree
import Data.Typeable

newtype DefaultSpace = DefaultSpace {_defaultSpace :: SpaceTree <: NoRev} deriving Typeable
latestSpace :: Lens DefaultSpace SpaceTree
latestSpace = defaultSpace . latestLens onNoRev
instance Space DefaultSpace where
  emptySpace = DefaultSpace $ Current emptySpace
  removeSpan s = modL lastestSpace $ removeSpan s
  addSpan s = modL lastestSpace $ addSpan s
  findSpan s = findSpan s . getL lastestSpace
  isFreeSpace s = isFreeSpace s . getL lastestSpace
  

makeLenses [''DefaultCount, ''DefaultSpace]
