{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, TupleSections, TemplateHaskell #-}

module Database.Perdure.Digest (
  Digest(..),
  Skein512Digest,
  MD5Digest,
  module Cgm.Data.LongWord
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Word
import Cgm.Data.LongWord
import Cgm.Data.Array
import Cgm.System.Endian
import System.IO.Unsafe
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.Skein512 as Skein512
--import qualified Database.Perdure.Skein512 as Skein512
import Cgm.Data.Structured

-- Recovers a hash function on words from a hash function on a specific byte encoding the words
-- Word type (size, endianness) must match that of the underlying algorithm.
-- Output size must be an integer number of words
unsafeBSToWordHash :: (Allocation f, PinnedArray r, Prim (ArrayElem r)) => (B.ByteString -> B.ByteString) -> r -> PrimArray f (ArrayElem r)
unsafeBSToWordHash f input = unsafePrimArrayCast $ arrayFromByteString $ unsafePerformIO $ unsafeWithCastArrayByteString (evaluate . f) input
  
-- Word size and specified endianness must match that of the underlying algorithm.
-- Output size must be an integer number of words
-- There will be no need for this method once we remove the byteswapping layer of the underlying algorithm, and we
-- have an underlying algorithm that assumes the input is in the platform endianness, instead of a fixed endianness.
unsafeFixedEndianToWordHash :: forall r. (PinnedArray r, ImmArray r, Endian (ArrayElem r), Prim (ArrayElem r)) => 
                               Endianness -> (B.ByteString -> B.ByteString) -> r -> PrimArray Free (ArrayElem r)
unsafeFixedEndianToWordHash e f = 
  if platformWordEndianness == e
  then unsafeBSToWordHash f
  else mapImmArray unswapBytes . (id :: Id (PrimArray Free (ByteSwapped w))) . 
       unsafeBSToWordHash f . (id :: Id (PrimArray Pinned (ByteSwapped (ArrayElem r)))) . 
       mapImmArray swapBytes

word128FromArray32LE :: PrimArray f Word32 -> Word128
word128FromArray32LE a = word128BE (retract splitWord64LE (indexArray a 2, indexArray a 3)) (retract splitWord64LE (indexArray a 0, indexArray a 1))

word128FromArray64LE :: PrimArray f Word64 -> Word128
word128FromArray64LE a = word128BE (indexArray a 1) (indexArray a 0)

newtype Skein512Digest h = Skein512Digest h deriving (Eq, Show)
newtype MD5Digest = MD5Digest Word128 deriving (Eq, Show)
  
class Eq d => Digest d where
  type DigestWord d
  digest :: (ImmArray r, PinnedArray r, ArrayElem r ~ DigestWord d) => r -> d

instance Digest (Skein512Digest Word128) where
  type DigestWord (Skein512Digest Word128) = Word64
  digest = {-# SCC "digestSkein" #-} (Skein512Digest . word128FromArray64LE . unsafeFixedEndianToWordHash LittleEndian (Skein512.hash 128))
  
instance Digest MD5Digest where
  type DigestWord MD5Digest = Word32
  digest = {-# SCC "digestMD5" #-} (MD5Digest . word128FromArray32LE . unsafeFixedEndianToWordHash LittleEndian MD5.hash)

deriveStructured ''MD5Digest
deriveStructured ''Skein512Digest
