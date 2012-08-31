{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, TemplateHaskell, ScopedTypeVariables #-}

module Database.Perdure.StoreFile (
    StoreFile(..),
    SyncableStoreFile(..),
    await0,
    await1,
    refSpan,
    Word64,
    BasicRef(..),
    module Foreign.Ptr,
    module Cgm.Data.Len,
    module Validator
) where

import Prelude ()
import Cgm.Prelude
import Data.Word
import Foreign.Ptr
import Cgm.Data.Len
import Cgm.Data.Super
import Cgm.Control.Concurrent.Await
import Database.Perdure.Validator as Validator
import Cgm.System.Endian
import Cgm.Data.Structured
import Database.Perdure.Space

class SyncableStoreFile f => StoreFile f where
  type StoreRef f :: * -> *
  -- | 
  storeFileWrite :: Endian w => f -> Len Word64 Word64 -> Endianness -> [PrimArray Pinned w] -> IO (StoreRef f w)
  storeFileRead :: (Validator v, ValidatedElem v ~ w, Endian w, LgMultiple w Word8) => 
                   f -> StoreRef f w -> Endianness -> v -> Async (Maybe (ArrayRange (PrimArray Pinned w))) ()
  
class SyncableStoreFile f where
  -- | Notify when all preceeding writes have completed, implies no barrier
  storeFileSync :: f -> IO () -> IO ()
  -- | Prevent the reordering of preceeding and subsequent read and write operations
  storeFileFullBarrier :: f -> IO ()
  
---------------------------------------------------------------------------
  
data BasicRef w = BasicRef {
    refStart :: {-# UNPACK #-} !(Len Word64 Word64),
    refSize :: {-# UNPACK #-} !(Len w Word32)}

refSpan :: forall w. LgMultiple Word64 w => BasicRef w -> Span
refSpan (BasicRef addr sz) = unsafeSortedPair (getLen addr) $ getLen $ (+ addr) $ coarsenLen (apply super sz :: Len w Word64)

deriveStructured ''BasicRef
