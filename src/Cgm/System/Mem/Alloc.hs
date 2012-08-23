{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Cgm.System.Mem.Alloc (
  mallocArrayLen,
  copyArrayLen,
  advancePtrLen,
  minusPtrLen,
  pokeLenOff,
  peekLenOff,
  hPutBufLen,
  hGetBufLen
  ) where

import Debug.Trace
import Data.Functor
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO
import Cgm.Data.Len
import Cgm.Control.Combinators

{-# INLINE mallocArrayLen #-}
mallocArrayLen :: (Storable a, Integral b) => Len a b -> IO (Ptr a)
mallocArrayLen = mallocArray . fromIntegral . getLen

{-# INLINE copyArrayLen #-}
copyArrayLen :: (Storable a, Integral b) => Ptr a -> Ptr a -> Len a b -> IO ()
copyArrayLen dest src = copyArray dest src . fromIntegral . getLen

{-# INLINE advancePtrLen #-}
advancePtrLen :: (Storable a, Integral b) => Ptr a -> Len a b -> Ptr a
advancePtrLen p = advancePtr p . fromIntegral . getLen

{-# INLINE minusPtrLen #-}
minusPtrLen :: (LgMultiple a Word8) => Ptr a -> Ptr a -> Len a Int
minusPtrLen a b = fst $ coarseRem (unsafeLen $ a `minusPtr` b :: Len Word8 Int)

{-# INLINE pokeLenOff #-}
pokeLenOff :: (Show a, Storable a, Integral b) => Ptr a -> Len a b -> a -> IO ()
pokeLenOff ptr = pokeElemOff ptr . fromIntegral . getLen

{-# INLINE peekLenOff #-}
peekLenOff :: (Storable a, Integral b) => Ptr a -> Len a b -> IO a
peekLenOff ptr = peekElemOff ptr . fromIntegral . getLen

{-# INLINE hPutBufLen #-}
hPutBufLen :: (Storable a, Integral b) => Handle -> Ptr a -> Len a b -> IO ()
hPutBufLen h ptr = hPutBuf h ptr . fromIntegral . getLen

{-# INLINE hGetBufLen #-}
hGetBufLen :: (Storable a, Integral b) => Handle -> Ptr a -> Len a b -> IO (Len a b)
hGetBufLen h ptr = ((unsafeLen . fromIntegral) <$>) . hGetBuf h ptr . fromIntegral . getLen