{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleInstances, 
FlexibleContexts, 
UndecidableInstances,
ScopedTypeVariables,
TypeSynonymInstances,
TypeOperators,
MultiParamTypeClasses, 
TypeFamilies, 
FunctionalDependencies, DeriveDataTypeable #-}

module Cgm.Data.WordN (
    RWord,
    toRWord,
    fromRWord,
    rWord,
    WordN(..),
    wordNBits,
    boolAsWord,
    RWordC,
    RWord8,
    RWord16,
    RWord32,
    RWord64,
    rChangeWord,
    rChangeWord',
    rAdd,
    rWordJoin,
    rWordSplit,
    rWordBool,
    module Cgm.Data.Nat,
    module Cgm.Data.Super
) where

import Data.Word
import Data.Int
import Data.Bits
import Control.Applicative
import GHC.Enum
import Cgm.Data.Bool
import Cgm.Data.Super
import Cgm.Data.Nat
import Cgm.Control.Combinators
import Cgm.Control.InFunctor
import Test.QuickCheck (Arbitrary(..), Gen)
import Data.Typeable

--testAddZ :: forall a. Nat a -> RWord (Add D0 (Nat a)) -> RWord (Nat a)
--testAddZ a r = addZ a r'
--	where r' :: (Add D0 (Nat a) ~ (Nat a)) => RWord (Nat a)
--              r' = r
--
--testRWordJoin :: forall a b. (NatClass a, NatClass b) => RWord (Nat a) -> RWord (Nat b) -> RWord (Add (Nat a) (Nat b))
--testRWordJoin a b = addComm (nat :: Nat a) (nat :: Nat b) $ addClosed (nat :: Nat b) (nat :: Nat a) $ \s -> rWordAdd (rWordJoin a b) $ rWordJoin b a
--
--runTestRWordJoin = print $ testRWordJoin (RWord 2 :: RWord D4) (RWord 3 :: RWord D5)

-- WordN

class (Bits w, Integral w, Nat (WordNBits w)) => WordN w where
    type WordNBits w :: *

instance WordN Word8 where type WordNBits Word8 = D8
instance WordN Word16 where type WordNBits Word16 = D16
instance WordN Word32 where type WordNBits Word32 = D32
instance WordN Word64 where type WordNBits Word64 = D64

wordNBits :: forall w. WordN w => w -> Int
wordNBits _ = (at :: At (WordNBits w)) intOfNat

-- RWord

newtype RWord w n = RWord {unRWord :: w} deriving Typeable

toRWord :: WordN w => w -> RWord w (WordNBits w)
toRWord = RWord

fromRWord :: RWord w n -> w
fromRWord = unRWord

rWord :: WordN w => Bijection' w (RWord w (WordNBits w))
rWord = uncheckedBijection toRWord fromRWord

boolAsWord :: Bijection' Bool (RWord Word8 D1)
boolAsWord = uncheckedBijection (bool 0 1) (== 1)

-- RWordC is just shorthand for an otherwise verbose context
class (WordN w, Nat n, Nat (WordNBits w :-: n)) => RWordC w n
instance (WordN w, Nat n, Nat (WordNBits w :-: n)) => RWordC w n

rWordJoin :: forall n m w. (Nat n, Nat m, Bits w) => RWord w n -> RWord w m -> Tagged n (RWord w (m :+: n))
rWordJoin (RWord un) (RWord um) = Tagged $ RWord $ um .|. (un `shiftL` (at :: At m) intOfNat)

rWordBool :: RWordC w D1 => RWord w D1 -> Bool
rWordBool = (`testBit` 0)

rWordSplit :: forall w n m. (RWordC w (n :+: m), RWordC w n, RWordC w m) => RWord w (n :+: m) -> Tagged m (RWord w m, RWord w n)
rWordSplit (RWord w) = Tagged $ (RWord (w `shiftR` (at :: At m) intOfNat), rClean $ RWord w)

rClean :: forall w n. RWordC w n => RWord w n -> RWord w n
rClean = liftRToR $ (.&.) $ (at :: At n) $ lowMaskNat

rSaturate :: forall w n. RWordC w n => RWord w n -> RWord w n
rSaturate = liftRToR $ (.|.) $ complement $ (at :: At n) $ lowMaskNat

-- TODO: The Nat ((WordNBits w) :-: n) is not used, so if it were defined as 'undefined' it would side-step out type checks (Nat only
-- closed if we exclude 'undefined' definitions). We could change the code so that it is used just to make sure it is not undefined.
-- Would representing the bound by a TBool valued type function be safer?
lowMaskNat :: RWordC w n => Tagged n w
lowMaskNat = (complement . shiftL (complement 0)) <$> intOfNat

-- TODO replace with more general versions in InFunctor, lift.... Oops no. That would no work. A Bijection cannot
-- be polymorphic. If we use a type function, like Structure, we could have more general lift functions.
liftRToA f = f . unRWord
liftAToR f = RWord . f
liftRToR f = RWord . f . unRWord
liftRRToR f = RWord ./ dot2i f unRWord -- could be written "wrapB rw rw rw", and we might then want to inline manually
liftRRToA f = dot2i f unRWord
liftRAToR f = RWord ./ dot2 f unRWord id
liftRRToRR f = (\(a, b) -> (RWord a, RWord b)) ./ dot2i f unRWord

-- Todo: remove Nat (WordNBits w' :-: n) which is redundant, it can be infered by transitivity: WordNBits w' >= n' >= n
--instance (RWordC w n, RWordC w' n', Nat (n' :-: n), Nat (WordNBits w' :-: n)) => Super (RWord w n) (RWord w' n') where 
--  super = rMoreBits `composeM` rChangeWord
--instance RWordC w n => Super (RWord w n) w where
--    super = uncheckedBijection unRWord RWord `composeM` (rMoreBits :: InjectionM' (RWord w n) (RWord w (WordNBits w)))
-- The above two commented instances are subsumed by the following instance, which might be less efficient

instance (RWordC w n, RWordC w' n) => Super (RWord w n) w' where
    super = uncheckedInjectionM fromIntegral (((RWord . fromIntegral) <$>) . predJust (<= bound)) where
      bound = fromIntegral (maxBound :: RWord w n) :: w'

rChangeWord :: forall w w' n . (RWordC w n, RWordC w' n) => Bijection' (RWord w n) (RWord w' n)
rChangeWord = uncheckedBijection (RWord . fromIntegral . unRWord) (RWord . fromIntegral . unRWord)

rChangeWord' :: forall w w' n . (RWordC w n, WordN w', Nat (WordNBits w' :-: WordNBits w)) => Bijection' (RWord w n) (RWord w' n)
rChangeWord' = uncheckedBijection (RWord . fromIntegral . unRWord) (RWord . fromIntegral . unRWord)

rMoreBits :: forall w n n' d . (RWordC w n, Nat d, (n' :-: n) ~ d) => InjectionM' (RWord w n) (RWord w n')
rMoreBits = uncheckedInjectionM (liftRToR id) ((RWord <$>) . predJust (<= (at :: At n) lowMaskNat) . unRWord)

rAdd :: Num w =>  RWord w n -> RWord w n -> RWord w (Succ n)
rAdd = liftRRToR (+)

instance RWordC w n => Bounded (RWord w n) where
    minBound = 0
    maxBound = (at :: At n) $ dupTag $ subI $ lowMaskNat

instance RWordC w n => Bits (RWord w n) where
    (.&.) = liftRRToR (.&.)
    (.|.) = liftRRToR (.|.)
    xor = liftRRToR xor
    complement = liftRToR (`xor` (at :: At n) lowMaskNat)
    shiftL = rClean ./ liftRAToR shiftL
    shiftR = liftRAToR shiftR
    rotateL x i = shiftL x i .|. shiftR x ((at :: At n) intOfNat - i)
    rotateR x i = rotateL x ((at :: At n) intOfNat - i)
    bitSize _ = (at :: At n) intOfNat
    isSigned _ = False

instance RWordC w n => Ord (RWord w n) where
    compare = liftRRToA compare

instance RWordC w n => Enum (RWord w n) where
  succ = rClean . liftRToR succ . rSaturate
  pred = liftRToR pred
  toEnum = rClean . liftAToR toEnum . ((+) $ fromIntegral $ complement $ ((at :: At n) lowMaskNat :: w))
  fromEnum = liftRToA fromEnum
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance RWordC w n => Real (RWord w n) where
    toRational = liftRToA toRational

instance RWordC w n => Integral (RWord w n) where
    quot = liftRRToR quot
    rem = liftRRToR rem
    div = liftRRToR div
    mod = liftRRToR mod
    quotRem = liftRRToRR quotRem
    divMod = liftRRToRR divMod
    toInteger = liftRToA toInteger

instance RWordC w n => WordN (RWord w n) where
    type WordNBits (RWord w n) = n

instance RWordC w n => Eq (RWord w n) where
    (==) = liftRRToA (==)

instance (Show w, RWordC w n) => Show (RWord w n) where
    show = liftRToA show

instance RWordC w n => Num (RWord w n) where
    (+) = rClean ./ liftRRToR (+)
    (-) = rClean ./ liftRRToR (-)
    (*) = rClean ./ liftRRToR (*)
    negate = rClean . liftRToR negate
    fromInteger = rClean . liftAToR fromInteger
    abs = liftRToR abs
    signum = liftRToR signum

instance (Arbitrary w, RWordC w n) => Arbitrary (RWord w n) where
  arbitrary = (rClean . RWord) <$> arbitrary
  shrink = (RWord <$>) . shrink . unRWord

type RWord8 = RWord Word8
type RWord16 = RWord Word16
type RWord32 = RWord Word32
type RWord64 = RWord Word64

--type RangeT n iMin eMax = (n :>=: iMin) :&&: (n :<: eMax)
--
--type RWord n = If (RangeT n D0 D8) (RWord Word8 n)
--                  (If (n :==: D8) Word8
--                      (If (RangeT n D9 D16) (RWord Word16 n)
--                          (If (n :==: D16) Word16
--                              (If (RangeT n D17 D32) (RWord Word32 n)
--                                  (If (n :==: D32) Word32
--                                      (If (RangeT n D33 D64) (RWord Word64 n)
--                                          (If (n :==: D64) Word64
--                                              ())))))))

