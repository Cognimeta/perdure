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

module Database.Perdure.AllocCopy (
  AllocCopy(..),
  allocCopyBits,
  module Database.Perdure.WriteBits
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Word
import Cgm.Data.Len
import Database.Perdure.WriteBits
import Cgm.System.Endian

class Endian w => AllocCopy w where
  allocCopyBitsSkip :: (BitSrc d, SrcDestState d ~ RealWorld) => Len w Word -> d -> d -> ST RealWorld (STPrimArray RealWorld Pinned w)

allocCopyBits :: (BitSrc d, SrcDestState d ~ RealWorld, AllocCopy w) => d -> d -> IO (PrimArray Pinned w)
allocCopyBits start end = stToIO $ allocCopyBitsSkip 0 start end >>= unsafeFreezeSTPrimArray

instance AllocCopy Word32 where
  allocCopyBitsSkip skip start end = onWordConv
                                     (apply wordConv1 <$> allocCopyBitsSkip (retract wordLenB skip) start end) 
                                     (error "allocCopyBitsSkip for Word32 not implemented") 
instance AllocCopy Word64 where
  allocCopyBitsSkip skip start end = onWordConv
                                     (error "allocCopyBitsSkip for Word64 not implemented") 
                                     (apply wordConv1 <$> allocCopyBitsSkip (retract wordLenB skip) start end) 

instance AllocCopy Word where
  -- Starts writing after the specified length 'skip', which can later be used to write a header.
  allocCopyBitsSkip skip start end = do
    wBuf <- mkArray $ coarsenLen (addedBits end start) + skip
    _ <- copyBits end start (aligned $ CWordSeq wBuf skip) >>= padIncompleteWord
    return wBuf
