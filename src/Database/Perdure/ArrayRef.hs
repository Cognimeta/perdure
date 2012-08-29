{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies #-}

module Database.Perdure.ArrayRef (
  ArrayRef(..),
  module Database.Perdure.Allocator
  ) where

import Database.Perdure.Allocator

class ArrayRef (r :: (* -> *) -> *) where
  type ArrayRefElem r
  writeArrayRef :: (Allocator l) => l -> PrimArray Pinned (ArrayRefElem r) -> IO (r (StoreRef ReplicatedFile))
  derefArrayRef :: (StoreFile f, Allocation fr) => f -> r (StoreRef f) -> IO (Maybe (ArrayRange (PrimArray fr (ArrayRefElem r))))
  arrayRefAddr ::  r BasicRef -> Len Word64 Word64
  
