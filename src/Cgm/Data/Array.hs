{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, UnboxedTuples, BangPatterns, MagicHash, TupleSections, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types, EmptyDataDecls #-}

module Cgm.Data.Array (
  ArrayIx,
  Array(..),
  ImmArray(..),
  STArray(..),
  STMkArray(..),
  STZArray(..),
  PinnedArray(..),
  Allocation(..),
  newAlignedPinnedWord8Array,
  withArrayByteString,
  arrayFromByteString,
  unsafeWithCastArrayByteString,
  Free,
  Pinned,
  primArrayMatchAllocation,
  PrimArray,
  runSTPrimArray,
  unsafeFreezeSTPrimArray,
  STPrimArray,
  ArrayRange(..),
  fullArrayRange,
  skipArrayRange,
  headArrayRange,
  wordConvArrayRange,
  unsafePrimArrayCast,
  unsafeSTPrimArrayCast,
  mapMArray,
  mapMArrayCopy,
  mapMArrayCopyImm,
  mapImmArray,
  mkArrayWith,
  ioToST,
  primSizeOf,
  ST(ST),
  module Control.Monad.ST,
  module Data.Primitive.Types
  ) where

import Prelude ()
import Cgm.Prelude
import GHC.IO( IO(..), ioToST )
import GHC.ST( ST(..) )
import GHC.Exts
import GHC.Prim
import qualified Data.ByteString as ByteString
import Data.ByteString.Unsafe
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.ST
import Cgm.Data.Word
import Cgm.Data.Len
import Cgm.System.Mem.Alloc
import Data.Primitive.Types
--import Cgm.System.Endian

type ArrayIx a = Len (ArrayElem a) Word
type ArrayST a = ST (ArrayState a)

class Array a where
  type ArrayElem a :: *
  arrayLen :: a -> ArrayIx a
  
class Array a => ImmArray a where
  indexArray :: a -> ArrayIx a -> ArrayElem a

class Array a => STArray a where
  type ArrayState a :: *

-- | STArrays that are instantiable in GCed memory
class STArray a => STMkArray a where
  mkArray :: ArrayIx a -> ArrayST a a

-- | STArrays that can be mutated using zero based indexing
class STArray a => STZArray a where
  readArray :: a -> ArrayIx a -> ArrayST a (ArrayElem a)
  writeArray :: a -> ArrayIx a -> ArrayElem a -> ArrayST a ()

class Array a => PinnedArray a where 
  withArrayPtr :: (Ptr (ArrayElem a) -> ArrayIx a -> IO b) -> a -> IO b

class Allocation f where
  newWord8Array :: Len Word8 Word -> ST s (STPrimArray s f Word8)
  onAllocation :: (f ~ Free => z) -> (f ~ Pinned => z) -> Tagged f z

withArrayByteString :: (PinnedArray a, ArrayElem a ~ Word8) => (ByteString.ByteString -> IO b) -> a -> IO b
withArrayByteString f a = flip withArrayPtr a $ \ptr len -> (unsafePackCStringFinalizer ptr (fromIntegral $ getLen len) $ return ()) >>= f

unsafeWithCastArrayByteString :: forall a b. (PinnedArray a, Prim (ArrayElem a)) => (ByteString.ByteString -> IO b) -> a -> IO b
unsafeWithCastArrayByteString f a = flip withArrayPtr a $ 
                                    \ptr len -> (unsafePackCStringFinalizer (castPtr ptr) (fromIntegral (getLen len) * primSizeOf (undefined :: ArrayElem a)) $ return ()) >>= f

-- Could we avoid the copy or do a memcpy ? Can we access the Ptr of the ByteString ?
arrayFromByteString :: Allocation f => ByteString.ByteString -> PrimArray f Word8
arrayFromByteString b = mkArrayWith (unsafeLen $ fromIntegral $ ByteString.length b) $ ByteString.index b . fromIntegral . getLen

arrayToList :: ImmArray a => a -> [ArrayElem a]
arrayToList a = fmap (indexArray a) $ counting $ arrayLen a
  
-- TODO remove Prim constraint, by adding method to get byte length of an array
immArrayEq :: (ImmArray a, Eq (ArrayElem a), Prim (ArrayElem a)) => a -> a -> Bool
immArrayEq a b = (arrayLen a == arrayLen b) && (and $ fmap (\i -> indexArray a i == indexArray b i) (counting $ min (arrayLen a) (arrayLen b)))

---------------------------------------------------------------------------
  
st_ :: (State# s -> State# s) -> ST s ()
st_ f = ST $ \s -> (# f s, () #)

---------------------------------------------------------------------------

data Free
data Pinned

instance Allocation Free where 
  newWord8Array l = ST $ \s -> case newByteArray# (unI# (fromIntegral $ getLen l)) s of (# s', a #) -> (# s', STPrimArray a #)
  onAllocation f p = tag f
instance Allocation Pinned where
  newWord8Array l = ST $ \s -> case newPinnedByteArray# (unI# (fromIntegral $ getLen l)) s of (# s', a #) -> (# s', STPrimArray a #)
  onAllocation f p = tag p

newAlignedPinnedWord8Array :: Len Word8 Word -> Len Word8 Word -> ST s (STPrimArray s f Word8)
newAlignedPinnedWord8Array al l = ST $ \s -> 
  case newAlignedPinnedByteArray# (unI# (fromIntegral $ getLen l)) (unI# (fromIntegral $ getLen al)) s of (# s', a #) -> (# s', STPrimArray a #)

primArrayMatchAllocation :: forall f f' w. (Allocation f, Allocation f', Prim w) => ArrayRange (PrimArray f w) -> ArrayRange (PrimArray f' w)
primArrayMatchAllocation = (at :: At f) $ onAllocation 
                           ((at :: At f') $ onAllocation id (fullArrayRange . mapImmArray id)) 
                           ((at :: At f') $ onAllocation (fullArrayRange . mapImmArray id) id)

---------------------------------------------------------------------------

primSizeOf :: Prim w => w -> Int  
primSizeOf w = I# (sizeOf# w)

-- | f is either Free or Pinned
data STPrimArray s f w = STPrimArray (MutableByteArray# s)

instance Eq (STPrimArray s f w) where (STPrimArray a) == (STPrimArray b) = sameMutableByteArray# a b

instance Prim w => Array (STPrimArray s f w) where
  type ArrayElem (STPrimArray s f w) = w
  arrayLen (STPrimArray a) = unsafeLen $ fromIntegral $ I# (sizeofMutableByteArray# a `quotInt#` sizeOf# (undefined :: w))

instance Prim w => STArray (STPrimArray s f w) where
  type ArrayState (STPrimArray s f w) = s

instance (Allocation f, Prim w) => STMkArray (STPrimArray s f w) where
  mkArray l = unsafeSTPrimArrayCast <$> newWord8Array (unsafeLen $ getLen l * fromIntegral (I# (sizeOf# (undefined :: w))))

instance Prim w => STZArray (STPrimArray s f w) where
  {-# INLINE readArray #-}
  readArray (STPrimArray a) i = ST $ readByteArray# a (word2Int# (unW# (getLen i)))
  {-# INLINE writeArray #-}
  writeArray (STPrimArray a) i w = st_ $ writeByteArray# a (word2Int# (unW# (getLen i))) w

instance Prim w => PinnedArray (STPrimArray s Pinned w) where
  {-# INLINE withArrayPtr #-}
  withArrayPtr f arr@(STPrimArray a) = f (Ptr (byteArrayContents# (unsafeCoerce# a))) (arrayLen arr) >>= (<$ (IO $ \s -> (# touch# a s, () #)))

---------------------------------------------------------------------------

-- | f is either Free or Pinned
data PrimArray f w = PrimArray ByteArray#

instance (Prim w, Eq w) => Eq (PrimArray f w) where (==) = immArrayEq
                                                    
instance (Prim w, Show w) => Show (PrimArray f w) where show = show . arrayToList

instance Prim w => Array (PrimArray f w) where
  type ArrayElem (PrimArray f w) = w
  arrayLen (PrimArray a) = unsafeLen $ fromIntegral $ I# (sizeofByteArray# a `quotInt#` sizeOf# (undefined :: w))

instance Prim w => ImmArray (PrimArray f w) where
  {-# INLINE indexArray #-}
  indexArray (PrimArray a) i = indexByteArray# a (word2Int# (unW# (getLen i)))

instance Prim w => PinnedArray (PrimArray Pinned w) where
  {-# INLINE withArrayPtr #-}
  withArrayPtr f arr@(PrimArray a) = f (Ptr (byteArrayContents# a)) (arrayLen arr) >>= (<$ (IO $ \s -> (# touch# a s, () #)))

runSTPrimArray :: (forall s . ST s (STPrimArray s f w)) -> PrimArray f w
runSTPrimArray st = runST (st >>= unsafeFreezeSTPrimArray)

-- | Safe as long as the input array is not written to after this call. Use the safe runSTPrimArray when possible.
unsafeFreezeSTPrimArray :: STPrimArray s f w -> ST s (PrimArray f w)
unsafeFreezeSTPrimArray (STPrimArray a) = ST (\s -> case unsafeFreezeByteArray# a s of (# s', a' #) -> (#s', PrimArray a' #) )

---------------------------------------------------------------------------

data ArrayRange a = ArrayRange a (ArrayIx a) (ArrayIx a) -- start and size

instance (ImmArray a, Eq (ArrayElem a), Prim (ArrayElem a)) => Eq (ArrayRange a) where (==) = immArrayEq
instance (ImmArray a, Show (ArrayElem a)) => Show (ArrayRange a) where show = show . arrayToList
instance Array a => Array (ArrayRange a) where 
  type ArrayElem (ArrayRange a) = ArrayElem a
  arrayLen (ArrayRange _ _ len) = len
instance ImmArray a => ImmArray (ArrayRange a) where  
  indexArray (ArrayRange a s _) i = indexArray a $ s + i
instance STArray a => STArray (ArrayRange a) where 
  type ArrayState (ArrayRange a) = ArrayState a
instance STZArray a => STZArray (ArrayRange a) where
  readArray (ArrayRange a s _) i = readArray a $ i + s
  writeArray (ArrayRange a s _) i e = writeArray a (i + s) e

instance (Prim (ArrayElem a), PinnedArray a) => PinnedArray (ArrayRange a) where 
  withArrayPtr f (ArrayRange a start sz) = withArrayPtr (\ptr _ -> f (ptr `plusPtrLen` start) sz) a

{-# INLINE plusPtrLen #-}
plusPtrLen :: forall a b. (Prim a, Integral b) => Ptr a -> Len a b -> Ptr a
plusPtrLen p = plusPtr p . (* primSizeOf (undefined :: a)) . fromIntegral . getLen

fullArrayRange :: Array a => a -> ArrayRange a
fullArrayRange a = ArrayRange a 0 (arrayLen a)

skipArrayRange :: Array a => ArrayIx a -> ArrayRange a -> ArrayRange a
skipArrayRange n (ArrayRange a s l) = ArrayRange a (s + n) (l - n)

headArrayRange :: Array a => ArrayIx a -> ArrayRange a -> ArrayRange a
headArrayRange n (ArrayRange a s l) = ArrayRange a s $ min n l

-- Can't be an instance of WordConv1 since ArrayRange has the wrong kind
wordConvArrayRange :: WordConv c => Bijection' (ArrayRange (PrimArray f Word)) (ArrayRange (PrimArray f c))
wordConvArrayRange = uncheckedBijection 
                       (\(ArrayRange a s l) -> ArrayRange (apply wordConv1 a) (apply wordLenB s) (apply wordLenB l))
                       (\(ArrayRange a s l) -> ArrayRange (retract wordConv1 a) (retract wordLenB s) (retract wordLenB l))

---------------------------------------------------------------------------

{-# INLINE unsafeSTPrimArrayCast #-}
unsafeSTPrimArrayCast :: STPrimArray s f w -> STPrimArray s f w'
unsafeSTPrimArrayCast (STPrimArray a) = STPrimArray a

{-# INLINE unsafePrimArrayCast #-}
unsafePrimArrayCast :: PrimArray f w -> PrimArray f w'
unsafePrimArrayCast (PrimArray a) = PrimArray a

instance WordConv1 (PrimArray f) where wordConv1 = uncheckedBijection unsafePrimArrayCast unsafePrimArrayCast
instance WordConv1 (STPrimArray s f) where wordConv1 = uncheckedBijection unsafeSTPrimArrayCast unsafeSTPrimArrayCast

{-# INLINE mapMArray #-}
mapMArray :: STZArray a => (ArrayElem a -> ArrayST a (ArrayElem a)) -> a -> ArrayST a ()
mapMArray f a = mapM_ (\i -> readArray a i >>= f >>= writeArray a i) $ counting $ arrayLen a

-- | The number of elements copied is the minimum of the number of elements in the source and the number of elements in the destination
{-# INLINE mapMArrayCopy #-}
mapMArrayCopy :: (STZArray a, STZArray b, ArrayState a ~ ArrayState b) => (ArrayElem a -> ArrayST a (ArrayElem b)) -> a -> b -> ArrayST a ()
mapMArrayCopy f src dest = mapM_ (\i -> readArray src (unsafeLen i) >>= f >>= writeArray dest (unsafeLen i)) $ 
                           counting $ min (getLen $ arrayLen src) (getLen $ arrayLen dest)

-- | The number of elements copied is the minimum of the number of elements in the source and the number of elements in the destination
{-# INLINE mapMArrayCopyImm #-}
mapMArrayCopyImm :: (ImmArray a, STZArray b) => (ArrayElem a -> ArrayST b (ArrayElem b)) -> a -> b -> ArrayST b ()
mapMArrayCopyImm f src dest = mapM_ (\i -> f (indexArray src $ unsafeLen i) >>= writeArray dest (unsafeLen i)) $ 
                           counting $ min (getLen $ arrayLen src) (getLen $ arrayLen dest)

mkArrayWith :: (Allocation f, Prim a) => Len a Word -> (Len a Word -> a) -> PrimArray f a
mkArrayWith l f = runSTPrimArray $ do
  m <- mkArray l 
  mapM_ (\i -> writeArray m i $ f i) $ counting l
  return m

mapImmArray :: (Allocation f', Prim b, ImmArray r) => (ArrayElem r -> b) -> r -> PrimArray f' b
mapImmArray f src = mkArrayWith (unsafeLen $ getLen $ arrayLen src) (f . indexArray src . unsafeLen . getLen)

{-

asSharedByteArray :: Endian w => STPrimArray s f w -> IO (STPrimArray s f Word8)
asSharedByteArray = unsafeSTPrimArrayCast

{-# INLINE destructivePlatformToLE #-}
destructivePlatformToLE :: (SizedArray (STPrimArray s f w), STZArray (STPrimArray s f w), Endian w) => STPrimArray s f w -> IO (STPrimArray s f w)
destructivePlatformToLE a = a <$ (littleEndianConv >>= maybe (return ()) (\c -> mapMArray (return . c) a))

-}
---------------------------------------------------------------------------

--instance Show (Buffer a) where show = ($ "") . showListWith showHex . ByteString.unpack . unsafePerformIO . bufferByteString
