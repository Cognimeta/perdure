{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Database.Perdure.Allocator (
  Allocator(..),
  allocWrite,
  module Database.Perdure.StoreFile,
  module Database.Perdure.LocalStoreFile,
  module Database.Perdure.SingleStoreFile,
  module Database.Perdure.ReplicatedFile
  ) where
  
import Prelude ()
import Cgm.Prelude
import Data.Word
import Database.Perdure.StoreFile
import Database.Perdure.LocalStoreFile
import Database.Perdure.SingleStoreFile
import Database.Perdure.ReplicatedFile
import Cgm.Data.Len
import Cgm.Data.Array
import Cgm.Data.Super
import Cgm.System.Endian
  
class Allocator l where 
  alloc :: l -> Len Word64 Word64 -> IO (Len Word64 Word64) -- take required size, and returns the address
  allocatorStoreFile :: l -> ReplicatedFile

allocWrite :: (Allocator l, LgMultiple Word64 w, Endian w) => l -> Endianness -> [PrimArray Pinned w] -> IO (StoreRef ReplicatedFile w)
allocWrite l e as = do
    addr <- alloc l $ apply super (coarsenLen $ sum $ arrayLen <$> as :: Len Word64 Word)
    storeFileWrite (allocatorStoreFile l) addr e as
