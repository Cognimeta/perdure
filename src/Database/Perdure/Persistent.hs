{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, 
  MultiParamTypeClasses, 
  FlexibleInstances, 
  FlexibleContexts, 
  TupleSections,
  TypeOperators,
  FunctionalDependencies, 
  GADTs, 
  GeneralizedNewtypeDeriving, 
  RankNTypes, 
  KindSignatures, 
  TemplateHaskell, 
  TypeFamilies, 
  UndecidableInstances, 
  DeriveFunctor, 
  DeriveDataTypeable,
  RoleAnnotations,
  StandaloneDeriving #-}

module Database.Perdure.Persistent (
  Persister(..),
  Persistent(..),
  Persistent1(..),
  Persistent1_(..),
  LgPersistent1_(..),
  RefPersister(..),
  RefPersistent(..),
  (&.),
  (|.),
  lenPersister,
  summationPersister,
  ratioPersister,  
  maybePersister,
  shortcutPersister,
  (>.),
  listPersister,
  ReplicatedFile,
  module Database.Perdure.CRef,
  DeserializerContext(..),
  DRef(..), --FIXME, we want to export constructor only to implementors
  WordArrayRef(..),
  WordNArrayRef(..),
  WArrayRef,
  IRef(..),
  Ref0(..),
  CDRef,
  Cache,
  module Cgm.Data.Structured,
  showDRef
) where

import Prelude ()
import Cgm.Prelude
import Data.Word
import Data.Int
import Data.Bool
import Cgm.Data.WordN
import Cgm.Data.Word
import Cgm.Data.Len
import Cgm.Data.Structured
import Cgm.Data.Functor.Sum
--import Database.Perdure.SoftRef
import Data.Ratio
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Calendar
import Data.Time.Clock
import Database.Perdure.Digest
import Database.Perdure.Validator
import Database.Perdure.WValidator
import Database.Perdure.StoreFile
import Database.Perdure.ReplicatedFile
import Database.Perdure.Cache(Cache)
import Cgm.System.Endian
import Data.Char
import Data.Binary.IEEE754
import Cgm.Data.List
import Data.Bits
import Database.Perdure.CRef
import Data.Dynamic
import Control.Concurrent.MVar
import Data.Functor.Identity
import qualified Data.ByteString as S

data WordNArrayRef v (r :: * -> *) = WordNArrayRef !v !(r (ValidatedElem v)) !Endianness
deriving instance (Show (r (ValidatedElem v)), Show v) => Show (WordNArrayRef v r)

data WordArrayRef r32 r64 (r :: * -> *) = Word32ArrayRef !(r32 r) | Word64ArrayRef !(r64 r) deriving Show

type WArrayRef = WordArrayRef (WordNArrayRef W32Validator) (WordNArrayRef W64Validator)

data DeserializerContext = forall f. (StoreFile f, StoreRef f ~ BasicRef) => DeserializerContext {dcFile :: f, dcCache :: MVar Cache}

data DRef a where
  DRef :: Typeable a => !(Persister a) -> !DeserializerContext -> !(WArrayRef BasicRef) -> DRef a deriving Typeable
showDRef :: DRef a -> String
showDRef (DRef p dc a) = "(DRef " ++ show a ++ " of type " ++ show (typeOfPersister p) ++ " )"

typeOfPersister :: forall a. Typeable a => Persister a -> TypeRep
typeOfPersister _ = typeOf (undefined :: a)

class LgPersistent1_ (r :: * -> *) where lgPersister1_ :: (LgMultiple Word64 w) => Persister (r w)

data Persister a where
  PartialWordPersister :: !(Len Bool Word) -> Persister Word -- Do not export to user
  PairPersister :: !(Persister a) -> !(Persister b) -> Persister (a, b)
  EitherPersister :: !(Persister a) -> !(Persister b) -> Persister (Either a b)
  ViewPersister :: !(InjectionA' a b) -> Persister b -> Persister a -- Not strict in Persister b because here we want to allow cycles
  SummationPersister :: !(Persister i) -> 
                        !(forall z. (forall b. Persister b -> (b -> a) -> z) -> i -> z) -> 
                        !(forall z. (forall b. i -> Persister b -> (b -> a) -> b -> z) -> a -> z) -> Persister a
  -- The index persister above should not contain refs because CSerializer uses the (b -> a), which ignores any storage changes to the i.
  -- The i is not stored as is in memory.
  DRefPersister' :: (Typeable a, Persistent a) => Persister (DRef a)
                    -- Persistent a instead of Persister a, multiple DRefs can point to the same location, so they must share the
                    -- same persister for a. Unless we were to support persistence of persisters, in which case the persister could
                    -- be encoded at the start of the referenced data (might be wasteful to always do this though). Persistence of
                    -- persisters means restricting ViewPersister to persistable views however (possibly only Structure), and
                    -- then we would also need to persist data with cycles.
  CRefPersister' :: (Typeable a, Persistent a) => !(RefPersister r) -> !(Persister (r a)) -> Persister (CRef r a)

instance InjectionACofunctor Persister where iacomap pb i = ViewPersister (injectionA' i) pb

infixr 7 &.
(&.) :: Persister a -> Persister b -> Persister (a, b)
(&.) = PairPersister

infixr 6 |.
(|.) :: Persister a -> Persister b -> Persister (Either a b)
(|.) = EitherPersister
  
summationPersister :: (Persister i) -> 
                        (forall z. (forall b. Persister b -> (b -> a) -> z) -> i -> z) -> 
                        (forall z. (forall b. i -> Persister b -> (b -> a) -> b -> z) -> a -> z) -> Persister a
summationPersister = SummationPersister  
  
data RefPersister r where
  Ref0Persister :: RefPersister Ref0
  RefView ::  (forall a. rb a -> ra a) -> RefPersister rb -> RefPersister ra
  SizeRefPersister :: Len Bool Word -> RefPersister (Sum Ref0 DRef)
  CRefPersister :: RefPersister r -> RefPersister (CRef r)
  DRefPersister :: RefPersister DRef
  IRefPersister :: RefPersister r -> RefPersister (IRef r)

class RefPersistent r where refPersister :: RefPersister r
                                                  
instance RefPersistent Ref0 where refPersister = Ref0Persister
instance RefPersistent r => RefPersistent (CRef r) where refPersister = CRefPersister refPersister
instance RefPersistent DRef where refPersister = DRefPersister
instance RefPersistent r => RefPersistent (IRef r) where refPersister = IRefPersister refPersister

class Persistent1_ (r :: * -> *) where persister1_ :: Persister (r a)
                                                       
class Persistent1 r where persister1 :: (Typeable a, Persistent a) => Persister (r a)
instance Persistent1 Ref0 where persister1 = structureMap persister
instance Persistent1 DRef where persister1 = DRefPersister'
instance Persistent1 r => Persistent1 (IRef r) where persister1 = structureMap persister1
instance (RefPersistent r, Persistent1 r) => Persistent1 (CRef r) where persister1 = CRefPersister' refPersister persister1
instance (Persistent1 ra, Persistent1 rb) => Persistent1 (Sum ra rb) where persister1 = structureMap $ persister1 |. persister1

-- Persistent

class Persistent a where persister :: Persister a

instance Persistent () where
  {-# INLINE persister #-}
  persister = PartialWordPersister 0 `iacomap` uncheckedInjectionA (\() -> 0) (const $ Just ())
instance Persistent Bool where
  {-# INLINE persister #-}
  persister = persister `iacomap` boolAsWord
instance Persistent Char where 
  {-# INLINE persister #-}
  persister = persister `iacomap` (uncheckedBijection (fromIntegral . ord) (chr . fromIntegral) :: Bijection' Char (RWord Word32 D21))
instance (Persistent a, Persistent b) => Persistent (Either a b) where
  {-# INLINE persister #-}
  persister = persister |. persister
instance (Persistent a1, Persistent a2) => Persistent (a1, a2) where
  {-# INLINE persister #-}
  persister = persister &. persister
instance (Persistent a1, Persistent a2, Persistent a3) => Persistent (a1,a2,a3) where persister = structureMap persister
instance (Persistent a1, Persistent a2, Persistent a3, Persistent a4) => Persistent (a1,a2,a3,a4) where persister = structureMap persister
instance (Persistent a1, Persistent a2, Persistent a3, Persistent a4, Persistent a5) => Persistent (a1,a2,a3,a4,a5) where 
  persister = structureMap persister
instance (Persistent a1, Persistent a2, Persistent a3, Persistent a4, Persistent a5, Persistent a6) => Persistent (a1,a2,a3,a4,a5,a6) where 
  persister = structureMap persister
instance Persistent Ordering where persister = structureMap persister
{-                                   
instance Persistent Word where 
  {-# INLINE persister #-}
  persister = wordPersister
-}
instance Persistent Word8 where 
  {-# INLINE persister #-}
  persister = unsafeBitsPersister
instance Persistent Word16 where 
  {-# INLINE persister #-}
  persister = unsafeBitsPersister
instance Persistent Word32 where 
  {-# INLINE persister #-}
  persister = onWordConv (wordPersister `iacomap` inv wordConv) unsafeBitsPersister
instance Persistent Word64 where 
  {-# INLINE persister #-}
  persister = onWordConv (persister `iacomap` splitWord64LE) (wordPersister `iacomap` inv wordConv)
{-                                   
instance Persistent Int where persister = persister `iacomap` unsigned
-}
instance Persistent Int8 where persister = persister `iacomap` unsigned
instance Persistent Int16 where persister = persister `iacomap` unsigned
instance Persistent Int32 where persister = persister `iacomap` unsigned
instance Persistent Int64 where persister = persister `iacomap` unsigned
instance Persistent Float where persister = persister `iacomap` uncheckedBijection floatToWord wordToFloat 
instance Persistent Double where persister = persister `iacomap` uncheckedBijection doubleToWord wordToDouble
instance Persistent Integer where persister = persister `iacomap` integerWords
instance (Integral a, Persistent a) => Persistent (Ratio a) where persister = ratioPersister persister
instance RWordC Word8 n => Persistent (RWord Word8 n) where persister = unsafeBitsPersister
instance RWordC Word16 n => Persistent (RWord Word16 n) where persister = unsafeBitsPersister
instance RWordC Word32 n => Persistent (RWord Word32 n) where persister = unsafeBitsPersister
instance RWordC Word64 n => Persistent (RWord Word64 n) where 
  persister = bool (onWordConv r64on32 unsafeBitsPersister) (PartialWordPersister n' `iacomap` unsafeIntegralAsWord) $ n' <= 32 where
      n' = (at :: At (RWord Word64 n)) bitSizeLen
      r64on32 = (persister &. partialWord32Persister) `iacomap` (injectionA' splitWord64LE . injectionA' super)
      partialWord32Persister = PartialWordPersister (n' - 32) `iacomap` unsafeIntegralAsWord
      -- spliting into a (Word32, RWord32) would be cleaner, but would require addSub as below, 
      -- and there might be some performance problems with that
instance Persistent a => Persistent (Maybe a) where persister = maybePersister persister
instance Persistent a => Persistent [a] where persister = listPersister persister
instance Persistent a => Persistent (Ref0 a) where persister = structureMap persister
instance Persistent W32Validator where persister = structureMap persister
instance Persistent W64Validator where persister = structureMap persister
instance Persistent Word128 where persister = structureMap persister
instance Persistent MD5Digest where persister = structureMap persister                                   
instance Persistent h => Persistent (Skein512Digest h) where persister = structureMap persister
instance (Persistent1 r, Typeable a, Persistent a) => Persistent (IRef r a) where persister = persister1
instance (Persistent1 ra, Persistent1 rb, Typeable a, Persistent a) => Persistent ((Sum ra rb) a) where persister = structureMap $ persister1 |. persister1
instance (RefPersistent r, Persistent1 r, Typeable a, Persistent a) => Persistent (CRef r a) where persister = persister1
instance Persistent a => Persistent (Len u a) where persister = lenPersister persister
instance (Ord k, Persistent k, Persistent v) => Persistent (Map.Map k v) where persister = persister `iacomap` (uncheckedInjection Map.toList Map.fromList)
instance (Ord k, Persistent k) => Persistent (Set.Set k) where persister = persister `iacomap` (uncheckedInjection Set.toList Set.fromList)
instance Persistent Day where persister = structureMap persister                            
instance Persistent UTCTime where persister = structureMap persister
instance Persistent DiffTime where persister = persister `iacomap` (uncheckedInjection toRational fromRational)
instance Persistent c => Persistent (() -> c) where
  persister = persister `iacomap` (uncheckedBijection ($ ()) const)
instance (Persistent (a -> c), Persistent (b -> c)) => Persistent (Either a b -> c) where
  persister = persister `iacomap` (uncheckedBijection (\f -> (f . Left, f . Right)) (\(fa, fb) -> either fa fb))

instance (Typeable a, Persistent a) => Persistent (DRef a) where persister = persister1
instance Persistent (BasicRef w) where persister = structureMap persister
instance LgPersistent1_ BasicRef where lgPersister1_ = persister
instance (Persistent v, LgPersistent1_ r, LgMultiple Word64 (ValidatedElem v)) => Persistent (WordNArrayRef v r) where 
  persister = structureMap $ persister &. lgPersister1_ &. (structureMap persister)
instance (Persistent (r32 r), Persistent (r64 r)) => Persistent (WordArrayRef r32 r64 r) where 
  persister = structureMap $ persister |. persister
instance Persistent a => Persistent (Identity a) where persister = persister `iacomap` uncheckedBijection runIdentity Identity
-- We inject into Either [Word8] ToBeDefined, so we can support an efficient encoding later on
instance Persistent S.ByteString where
  persister = persister `iacomap` (uncheckedInjection (Left . S.unpack) (either S.pack (error "Invalid ByteString" :: () -> S.ByteString)))

{-# INLINE listPersister #-}  
-- | Persister for lists built from a specified element persister.
listPersister :: List a => Persister (Listed a) -> Persister a
listPersister elemPersister = (maybePersister $ elemPersister &. listPersister elemPersister) `iacomap` listStructure

-- | Takes persisters for 2 types, and an injection from the smaller type 'a' to the larger type 'b', and gives a
-- persister for the larger type which uses the smaller type representation when possible, plus one bit to identify
-- which representation is used.
shortcutPersister :: InjectionM i => i a b -> Persister b -> Persister a -> Persister b
shortcutPersister i b a = (b |. a) `iacomap` eitherI where
  eitherI = uncheckedInjection (\x -> ($ x) $ maybe (Left x) Right . unapply i) (either id $ apply i)

-- | Specialization of shortcutPersister with the 'super' injection.
infixl 9 >.
(>.) :: Super a b => Persister b -> Persister a -> Persister b
(>.) = shortcutPersister super

-- | Persister for 'Maybe a' built from a specified 'a' persister. Uses a single bit to represent 'Nothing'.
maybePersister :: Persister a -> Persister (Maybe a)
maybePersister elemPersister = structureMap $ persister |. elemPersister

--bitPersister :: Persister Word -- Do not export to user
--bitPersister = PartialWordPersister 1

wordPersister :: Persister Word -- Do not export to user, Word is platform dependent
wordPersister = PartialWordPersister wordBits

-- prefer wordPersister when writing a full word
-- unsafe because (finiteBitSize a) size must not exceed Word size
unsafeBitsPersister :: forall a. (FiniteBits a, Integral a) => Persister a
unsafeBitsPersister = PartialWordPersister ((at :: At a) bitSizeLen) `iacomap` unsafeIntegralAsWord

-- unsafe because (bitSize a) size must not exceed Word size
unsafeIntegralAsWord :: Integral a => Injection' a Word
unsafeIntegralAsWord = uncheckedInjection fromIntegral fromIntegral

lenPersister :: Persister a -> Persister (Len u a)
lenPersister = structureMap

{-# INLINE ratioPersister #-}  
ratioPersister :: Integral a => Persister a -> Persister (Ratio a)
ratioPersister elemPersister =
  (elemPersister &. elemPersister) `iacomap` (uncheckedBijection (numerator &&& denominator) (uncurry (%)))

integerWords :: Bijection' Integer (Int32, [Word32])
integerWords = uncheckedBijection
               (unfoldlE $ \i -> let o = fromIntegral i in if fromIntegral o == i then Left o else Right (i `shiftR` 32, fromIntegral i))
               (\(i, l) -> foldl (\x d -> (x `shiftL` 32) + fromIntegral d) (fromIntegral i) l)


-- IRef

newtype IRef r t = IRef {getIRef :: r t} deriving (Functor, Applicative)

-- Ref0
                                                    
newtype Ref0 a = Ref0 a deriving (Functor, Eq)

-- Temporary Aliases
type CDRef = CRef DRef

deriveStructured ''Ref0
deriveStructured ''IRef
deriveStructured ''Day
deriveStructured ''UTCTime
deriveStructured ''WordNArrayRef                                                               
deriveStructured ''WordArrayRef

