{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, UnboxedTuples, BangPatterns, MagicHash, FlexibleInstances, TupleSections, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

-- Command line used to see simplified Core (from /src dir)
-- ghc-7.0.1 -fforce-recomp  --make -O2 Cgm/Persist/WriteBits.hs -ddump-simpl -dppr-user-length=130 > b.hcr
-- Notes on status: despite wordSeqAdd being inlined, the simplifier does not automatically create versions
-- of writeBits functions that take the real-world, and return the real world as an additional element in the
-- returned unboxed tuple. It still return an IO in the returned unboxed tuple. Will we need to do this manually ?
-- Will subsequent optimization take care of that? Probably not since the IO monad is lazy.

module Database.Perdure.WriteBits ( 
  STSrcDest(..),
  WordDest(..),
  BitDest(..),
  BitSrc(..),
  WordSrc(..),
  aligned,
  padIncompleteWord,
  WordSeq,
  mkWordSeq,
  CWordSeq(..),
  BitAcc(..),
  bitAccWordDest,
  BSer,
  ABitSeq,
  mkABitSeq,
  module Cgm.Data.Array
  ) where

import Debug.Trace
import Foreign.Ptr
import Data.Bits
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Cgm.Data.Array
import Cgm.Data.Word
import Cgm.Data.Len
import Cgm.Data.WordN
import Cgm.System.Mem.Alloc
import Cgm.Control.Combinators


------------------------- classes WordDest, BitDest, BitSrc and WordSrc -------------------------

class STSrcDest d where
  type SrcDestState d
type SrcDestST d = ST (SrcDestState d)
  
class STSrcDest d => WordDest d where
  addWord :: Word -> d -> SrcDestST d d

class WordDest d => BitDest d where
  addBit :: Word -> d -> SrcDestST d d -- 0 <= w <= 1
  addBits :: Len Bool Word -> Word -> d -> SrcDestST d d -- 0 <= n <= wordBits, w < 2^n

class STSrcDest d => BitSrc d where
  addedBits :: d -> d -> Len Bool Word
  copyBits :: (BitDest d', SrcDestST d ~ SrcDestST d') => d -> d -> d' -> SrcDestST d d'
              -- ^ Copies all bits between an end and a start (of type d) to a destination

class BitSrc d => WordSrc d where
  addedWords :: d -> d -> Len Word Word
  copyWords :: (WordDest d', SrcDestST d ~ SrcDestST d') => d -> d -> d' -> SrcDestST d d' 
               -- ^ Copies all words between an end and a start (of type d) to a destination
  copyWordsPartial :: (BitDest d', SrcDestST d ~ SrcDestST d') => d -> d -> Len Bool Word -> d' -> SrcDestST d d' 
                      -- ^ Like copyWords, but excludes some lower bits [0, wordSize[ from the first Word
  
------------------------- Ptr -------------------------

instance STSrcDest (Ptr Word) where type SrcDestState (Ptr Word) = RealWorld
instance WordDest (Ptr Word) where
  {-# INLINE addWord #-}
  addWord w ptr = ioToST $ advancePtr ptr 1 <$ poke ptr w
instance BitSrc (Ptr Word) where
  addedBits = refineLen ./ addedWords
  copyBits = copyWords
instance WordSrc (Ptr Word) where
  addedWords = (apply unsigned <$>) ./ minusPtrLen
  copyWords end start d = if (end == start) then return d else ioToST (peek start) >>= \w -> addWord w d >>= copyWords end (start `advancePtrLen` 1)
  copyWordsPartial end start dropLow = 
    let len = addedWords end start 
    in if (len == 0) 
       then undefined 
       else (\d -> ioToST (peek start) >>= \w -> 
              addBits (refineLen word - dropLow) (w `partialShiftRL` getLen dropLow) d) >=> copyWords end (start `advancePtrLen` 1)

------------------------- WordSeq -------------------------

-- With WordSeq we allocate memory as we go, so we do not need a size bound ahead of time.
-- Here the least significant bits of the words are bits that were added before.
-- 0 <= index < chunkSize
data WordSeq s f = WordSeq !((Len Word Word), [STPrimArray s f Word]) {-# UNPACK #-} !(CWordSeq s f)

chunkSize :: Len Word Word
chunkSize = unsafeLen 2048

{-# INLINE pushAnyFullChunk #-}
pushAnyFullChunk :: STMkArray (STPrimArray s f Word) => WordSeq s f -> ST s (WordSeq s f)
pushAnyFullChunk s@(WordSeq (l, r) (CWordSeq a ix)) = 
  if (ix < chunkSize) then return s else WordSeq (l + chunkSize, a : r) . (`CWordSeq` 0) <$> mkArray chunkSize
                                                                                 
mkWordSeq :: STMkArray (STPrimArray s f Word) => ST s (WordSeq s f)
mkWordSeq = WordSeq (0, []) . (`CWordSeq` 0) <$> mkArray chunkSize

instance STSrcDest (WordSeq s f) where type SrcDestState (WordSeq s f) = s
instance STMkArray (STPrimArray s f Word) => WordDest (WordSeq s f) where                          
  {-# INLINE addWord #-}
  addWord w (WordSeq r c) = (WordSeq r <$> addWord w c) >>= pushAnyFullChunk
instance BitSrc (WordSeq s f) where
  addedBits = refineLen ./ addedWords
  copyBits = copyWords
instance WordSrc (WordSeq s f) where
  addedWords (WordSeq (s1, _) c1) (WordSeq (s0, _) c0) = addedWords c1 c0 + (s1 - s0)
  copyWords (WordSeq (s1, r1) c1) start@(WordSeq (s0, _) c0) =
    if (s1 == s0)
    then copyWords c1 c0
    else copyWords (WordSeq (s1 - chunkSize, tail r1) $ chunkEnd (head r1)) start >=> copyWords c1 (chunkStart c1)
  copyWordsPartial (WordSeq (s1, r1) c1) start@(WordSeq (s0, _) c0) =
    if (s1 == s0)
    then copyWordsPartial c1 c0
    else \drop -> copyWordsPartial (WordSeq (s1 - chunkSize, tail r1) $ chunkEnd (head r1)) start drop >=> copyWords c1 (chunkStart c1)
 
chunkStart (CWordSeq a i) = CWordSeq a 0
chunkEnd a = CWordSeq a chunkSize
  
------------------------- CWordSeq -------------------------

-- Like WordSeq, but all words are contiguous, so the sequence cannot grow. No boundary check is performed. The initial allocation must be sufficient.
data CWordSeq s f = CWordSeq {-# UNPACK #-} !(STPrimArray s f Word) {-# UNPACK #-} !(Len Word Word)

instance STSrcDest (CWordSeq s f) where type SrcDestState (CWordSeq s f) = s
instance WordDest (CWordSeq s f) where                          
  {-# INLINE addWord #-}
  addWord w (CWordSeq arr ix) = CWordSeq arr (ix + 1) <$ writeArray arr ix w
instance BitSrc (CWordSeq s f) where
  addedBits = refineLen ./ addedWords
  copyBits = copyWords
instance WordSrc (CWordSeq s f) where
  addedWords (CWordSeq _ n1) (CWordSeq _ n0) = n1 - n0
  copyWords end@(CWordSeq a i1) (CWordSeq _ i0) d =
    if (i1 == i0) then return d else readArray a i0 >>= flip addWord d >>= copyWords end (CWordSeq a $ i0 + 1)
  copyWordsPartial end@(CWordSeq a i1) (CWordSeq _ i0) dropLow d =
    if (i1 == i0) then undefined 
    else do 
      w <- readArray a i0
      addBits (refineLen word - dropLow) (w `partialShiftRL` getLen dropLow) d >>= copyWords end (CWordSeq a $ i0 + 1)
 
------------------------- BitAcc -------------------------

-- Invariant : 0 <= b < wordBits, w < 2^b
data BitAcc d = BitAcc {-# UNPACK #-} !Word {-# UNPACK #-} !Word d

type BSer d = BitAcc d -> SrcDestST (BitAcc d) (BitAcc d)

{-# INLINE onAlignment #-}
onAlignment :: (d -> z) -> (BitAcc d -> z) -> BitAcc d -> z
onAlignment a u bd@(BitAcc b _ d) = if (b == 0) then a d else u bd

aligned :: d -> BitAcc d
aligned = BitAcc 0 0

bitAccWordDest :: BitAcc d -> d
bitAccWordDest (BitAcc _ _ d) = d

padIncompleteWord :: WordDest d => BitAcc d -> SrcDestST d d
padIncompleteWord = onAlignment return (\(BitAcc _ acc d) -> addWord acc d)

instance STSrcDest d => STSrcDest (BitAcc d) where type SrcDestState (BitAcc d) = SrcDestState d
instance WordDest d => WordDest (BitAcc d) where
  {-# SPECIALIZE INLINE addWord :: Word -> BSer (WordSeq RealWorld Free) #-}  
  {-# SPECIALIZE INLINE addWord :: Word -> BSer (Ptr Word) #-}
  addWord !w (BitAcc b acc d) = BitAcc b (if b == 0 then 0 else partialShiftRL w (wordBits - b)) <$> addWord (acc + partialShiftL w b) d

instance WordDest d => BitDest (BitAcc d) where
  {-# SPECIALIZE INLINE addBits :: Len Bool Word -> Word -> BSer (WordSeq s Free) #-}  
  {-# SPECIALIZE INLINE addBits :: Len Bool Word -> Word -> BSer (Ptr Word) #-}  
  addBits n !w (BitAcc b acc d) = let 
    !acc' = acc + partialShiftL w b
    b' = b + getLen n
    b'' = b' - wordBits
    in if (signed $* b'') < 0
       then return $ BitAcc b' acc' d
       else let 
         !fullShift = getLen n == wordBits && b == 0
         in if fullShift then aligned <$> addWord acc' d else BitAcc b'' (partialShiftRL w (getLen n - b'')) <$> addWord acc' d
  {-# SPECIALIZE INLINE addBit :: Word -> BSer (WordSeq s Free) #-}  
  {-# SPECIALIZE INLINE addBit :: Word -> BSer (Ptr Word) #-}  
  addBit !w (BitAcc b acc d) = let 
    acc' = acc + partialShiftL w b
    b' = b + 1
    b'' = b' - wordBits
    in if (signed $* b'') < 0
       then return $ BitAcc b' acc' d
       else aligned <$> addWord acc' d
instance WordSrc d => BitSrc (BitAcc d) where
  copyBits (BitAcc endB endAcc end) (BitAcc startB _ start) =
    if (addedWords end start == 0) 
    then addBits (unsafeLen $ endB - startB) (endAcc `partialShiftRL` startB)
    else copyWordsPartial end start (unsafeLen startB) >=> addBits (unsafeLen endB) endAcc
  addedBits (BitAcc b1 _ d1) (BitAcc b0 _ d0) = refineLen (addedWords d1 d0) + unsafeLen (b1 - b0)


type ABitSeq s = BitAcc (WordSeq s Free)
mkABitSeq :: ST s (ABitSeq s)
mkABitSeq = aligned <$> mkWordSeq
