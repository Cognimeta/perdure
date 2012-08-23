{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Database.Perdure.TestStoreFile (
  testStoreFile,
  testFile
  ) where

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Array
import Database.Perdure.StoreFile
import Database.Perdure.LocalStoreFile
import Database.Perdure.SingleStoreFile
import Cgm.Data.Word
import Cgm.Data.Either
import Cgm.Data.Len
import Cgm.Data.Array
import Cgm.System.Endian
import Control.Exception
import Control.Monad.Error

testStoreFile :: [String] -> IO ()
testStoreFile args = fmap fromRight $ runErrorT $ withFileStoreFile "testStoreFile.dag" $ (. SingleStoreFile) $
    \f -> do
      let a :: PrimArray Pinned Word32 = mkArrayWith 130000 $ fromIntegral . getLen
      r <- storeFileWrite f 0 platformWordEndianness [a]
      storeFileFullBarrier f
      ma <- await1 $ storeFileRead f r platformWordEndianness Validator0
      assert (ma == Just (fullArrayRange a)) $ return ()

