{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE MagicHash, TypeFamilies, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Cgm.Data.Word (
  WordConv(..),
  onWordConv,
  onWordConvB,
  wordBits,
  partialShiftL,
  partialShiftRL,
  uShiftL,
  uShiftRL,
  unI#,
  unW#,
  Signed(..),
  unsigned,
  WordConv1(..),
  splitWord64LE,
  module Data.Word
  ) where

import Prelude()
import Cgm.Prelude
import Control.Category
import Data.Bits
import Data.Int
import Data.Word
import GHC.Prim
import GHC.Exts
import Cgm.Control.InFunctor
import Cgm.Data.Tagged
import Cgm.Data.WordN
import Cgm.Control.Combinators

{-# INLINE wordBits #-}
wordBits :: Integral a => a
wordBits = fromIntegral $ bitSize (undefined :: Word)


class WordConv a where
  wordConv  :: Bijection' Word a

onWordConvB :: (Bijection' Word Word32 -> z) -> (Bijection' Word Word64 -> z) -> z
onWordConvB a b = onWordConv (a wordConv) (b wordConv)

onWordConv :: (WordConv Word32 => z) -> (WordConv Word64 => z) -> z

#if WORDSIZE == 32
instance WordConv Word32 where
  wordConv = uncheckedBijection fromIntegral fromIntegral
onWordConv a _ = a
#endif

#if WORDSIZE == 64
instance WordConv Word64 where
  wordConv = uncheckedBijection fromIntegral fromIntegral
onWordConv _ a = a
#endif

-- | 0 <= n < wordBits
{-# INLINE partialShiftL #-}
partialShiftL :: Word -> Word -> Word
partialShiftL (W# w) (W# n) = W# (w `uncheckedShiftL#` word2Int# n)

-- | 0 <= n < wordBits
{-# INLINE partialShiftRL #-}
partialShiftRL :: Word -> Word -> Word
partialShiftRL (W# w) (W# n) = W# (w `uncheckedShiftRL#` word2Int# n)

{-# INLINE uShiftL #-}
uShiftL :: Word -> Word -> Word
uShiftL w n = if n >= wordBits then 0 else partialShiftL w n

{-# INLINE uShiftRL #-}
uShiftRL :: Word -> Word -> Word
uShiftRL w n = if n >= wordBits then 0 else partialShiftRL w n

{-# INLINE unW# #-}
unW# (W# a) = a
{-# INLINE unI# #-}
unI# (I# a) = a


class Signed u s | u -> s, s -> u where signed :: Bijection' u s
instance Signed Word8 Int8 where 
  {-# INLINE signed #-}
  signed = uncheckedBijection fromIntegral fromIntegral
instance Signed Word16 Int16 where 
  {-# INLINE signed #-}
  signed = uncheckedBijection fromIntegral fromIntegral
instance Signed Word32 Int32 where 
  {-# INLINE signed #-}
  signed = uncheckedBijection fromIntegral fromIntegral
instance Signed Word64 Int64 where 
  {-# INLINE signed #-}
  signed = uncheckedBijection fromIntegral fromIntegral
instance Signed Word Int where 
  {-# INLINE signed #-}
  signed = uncheckedBijection fromIntegral fromIntegral

{-# INLINE unsigned #-}
unsigned :: Signed u s => Bijection' s u
unsigned = inv signed

class WordConv1 a where
  wordConv1 :: WordConv c => Bijection' (a Word) (a c)

splitWord64LE :: Bijection' Word64 (Word32, Word32)
splitWord64LE = uncheckedBijection (fk (,) fromIntegral $ fromIntegral . (`shiftR` 32)) (\(l, h) -> fromIntegral h `shiftL` 32 + fromIntegral l)
