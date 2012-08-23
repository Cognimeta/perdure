{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, TupleSections, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}

module Database.Perdure.RootValidator (
  RootValidator(..),
  ValidationDigestWord(..),
  module Database.Perdure.Validator
  ) where

import Data.Word
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Binary as Binary
import Crypto.Hash.Tiger
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe
import Cgm.Data.Digest
import Cgm.Data.Maybe
import Cgm.Data.Len
import Database.Perdure.Validator
import Database.Perdure.CSerializer
import Database.Perdure.CDeserializer
import Cgm.Control.Concurrent.Await
import Cgm.Control.Combinators
import Database.Perdure.AllocCopy

-- ValidationDigest assiciates a particular digest method to each word type to digest (Word32 and Word64)
-- It must be a right inverse of DigestWord.
-- We'd need a superclass equality constraint here.
-- As a first wordaround attempt we place the equality constraint in instances
-- Users fail with an infinite type error.
-- Read a relevant post: http://www.haskell.org/pipermail/haskell-cafe/2009-January/053696.html                                                                  
-- Actual workaround we used: removed Digest from superclass of ValidationDigestWord, added digest' method which delegates to digest
class (AllocCopy w, Deserializable w, LgMultiple w Bool, LgMultiple w Word8, LgMultiple Word64 w, Persistent (ValidationDigest w), 
       Eq (ValidationDigest w), Show (ValidationDigest w)) => ValidationDigestWord w where 
  type ValidationDigest w
  digest' :: (ImmArray r, PinnedArray r, ArrayElem r ~ w) => r -> ValidationDigest w
instance ValidationDigestWord Word32 where
  type ValidationDigest Word32 = MD5Digest
  digest' = digest
instance ValidationDigestWord Word64 where 
  type ValidationDigest Word64 = Skein512Digest Word128
  digest' = digest

data RootValidator w = RootValidator deriving (Eq, Show)
type Header w = (Len w Word, ValidationDigest w)
instance (ValidationDigestWord w, Show (ValidationDigest w)) => Validator (RootValidator w) where
  type ValidatedElem (RootValidator w) = w
  mkValidationInput b = (RootValidator, [serializeToArray persister ((arrayLen b, digest' b) :: Header w), b])
  validate RootValidator b = 
    let DeserOut ((len, h) :: Header w) u = deserializeFromArray (unsafeSeqDeserializer persister) b
        payload = headArrayRange len $ skipArrayRange (coarsenLen u) b
    in payload `justIf` (digest' payload == h)

instance Persistent (RootValidator w) where persister = structureMap persister

deriveStructured ''RootValidator