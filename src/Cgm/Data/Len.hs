{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving, DeriveFunctor, DeriveDataTypeable  #-}

module Cgm.Data.Len (
  Len,
  showLen,
  bitLen,
  word8,
  word16,
  word32,
  word64,
  word,
  LgMultiple(..),
  unsafeLen,
  getLen,
  wordLenB,
  bitSizeLen,
  refineLen,
  coarsenLen,
  coarseRem,
  ceilDivPower2,
  roundUpPower2
  ) where

import Prelude()
import Cgm.Prelude
import Data.Bits
import Control.Category
import Cgm.Data.Tagged
import Cgm.Data.Word
import Cgm.Data.WordN
import Cgm.Control.InFunctor
import Cgm.Data.Structured
import Cgm.Data.Super
import Data.Typeable

newtype Len u n = Len {getLen :: n} deriving (Ord, Enum, Real, Integral, Num, Eq, Bounded, Functor, Typeable)

instance Show n => Show (Len u n) where show = show . getLen

showLen :: forall n u. (Show n, LgMultiple u Bool) => Len u n -> String 
showLen (Len n) = show n ++ "_" ++ show ((1::Int) `shiftL` getLgMul (lgMul :: LgMul u Bool))

unsafeLen = Len

bitLen :: Integral a => Len Bool a
bitLen = Len 1
word8 :: Integral a => Len Word8 a
word8 = Len 1
word16 :: Integral a => Len Word16 a
word16 = Len 1
word32 :: Integral a => Len Word32 a
word32 = Len 1
word64 :: Integral a => Len Word64 a
word64 = Len 1
word :: Integral a => Len Word a
word = Len 1

-- Says that type a's size is a (non-negative) power of 2 multiple of type b's size
newtype LgMul a b = LgMul {getLgMul :: Int}
class LgMultiple a b where lgMul :: LgMul a b
instance LgMultiple Word8 Bool where lgMul = LgMul 3
instance LgMultiple Word16 Word8 where lgMul = LgMul 1
instance LgMultiple Word32 Word16 where lgMul = LgMul 1
instance LgMultiple Word64 Word32 where lgMul = LgMul 1
-- Reflexive instances
instance LgMultiple Bool Bool where lgMul = id                                        
instance LgMultiple Word8 Word8 where lgMul = id                                        
instance LgMultiple Word16 Word16 where lgMul = id                                        
instance LgMultiple Word32 Word32 where lgMul = id                                        
instance LgMultiple Word64 Word64 where lgMul = id
-- Transitive closure
instance LgMultiple Word16 Bool where lgMul = (at :: At Word8) lgMulTrans
instance LgMultiple Word32 Bool where lgMul = (at :: At Word8) lgMulTrans
instance LgMultiple Word64 Bool where lgMul = (at :: At Word8) lgMulTrans
instance LgMultiple Word32 Word8 where lgMul = (at :: At Word16) lgMulTrans
instance LgMultiple Word64 Word8 where lgMul = (at :: At Word16) lgMulTrans
instance LgMultiple Word64 Word16 where lgMul = (at :: At Word32) lgMulTrans
-- Word instances                                      
instance LgMultiple Word Bool where lgMul = wordLgMul
instance LgMultiple Word Word8 where lgMul = wordLgMul
instance LgMultiple Word Word16 where lgMul = wordLgMul
instance LgMultiple Word Word32 where lgMul = wordLgMul
instance LgMultiple Word64 Word where lgMul = wordLgMul'

wordLgMul :: forall a. (LgMultiple Word32 a, LgMultiple Word64 a) => LgMul Word a
wordLgMul = onWordConv (LgMul $ getLgMul (lgMul :: LgMul Word32 a)) (LgMul $ getLgMul (lgMul :: LgMul Word64 a))

wordLgMul' :: forall a. (LgMultiple a Word32, LgMultiple a Word64) => LgMul a Word
wordLgMul' = onWordConv (LgMul $ getLgMul (lgMul :: LgMul a Word32)) (LgMul $ getLgMul (lgMul :: LgMul a Word64))

instance Category LgMul where
  id = LgMul 0
  LgMul a . LgMul b = LgMul $ a + b
  
lgMulTrans :: forall a b c. (LgMultiple a b, LgMultiple b c) => Tagged b (LgMul a c)
lgMulTrans = tag $ lgMul . (at :: At b) idAt . lgMul

idAt :: Category c => Tagged a (c a a)
idAt = tag id

bitSizeLen :: forall a b. (Bits a, Integral b) => Tagged a (Len Bool b)
bitSizeLen = tag $ Len $ fromIntegral $ bitSize (undefined :: a)

wordLenB :: WordConv a => Bijection' (Len Word b) (Len a b)
wordLenB = uncheckedBijection (Len . getLen) (Len . getLen)

refineLen :: forall u v n. (Bits n, LgMultiple u v) => Len u n -> Len v n
refineLen (Len u) = Len $ u `shiftL` getLgMul (lgMul :: LgMul u v)
coarsenLen :: forall u v n. (Bits n, LgMultiple v u) => Len u n -> Len v n
coarsenLen (Len u) = Len $ ceilDivPower2 (getLgMul (lgMul :: LgMul v u)) u

{-# INLINE coarseRem #-}
coarseRem ::  forall u v n. (Bits n, LgMultiple v u) => Len u n -> (Len v n, Len u n)
coarseRem (Len u) = let s = getLgMul (lgMul :: LgMul v u) in (Len $ u `shiftR` s, Len $ u .&. ((1 `shiftL` s) - 1))

ceilDivPower2 n x = (x + ((1 `shiftL` n) - 1)) `shiftR` n
roundUpPower2 n x = ceilDivPower2 n x `shiftL` n

instance Super a b => Super (Len u a) (Len u b) where super = injectionM' (inv struct) . super . injectionM' struct

deriveStructured ''Len
