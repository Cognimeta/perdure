{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Database.Perdure.WordArrayRef (
  WordArrayRef(..)
  ) where

import Prelude()
import Cgm.Prelude
import Cgm.Data.Word
import Database.Perdure.ArrayRef
import Database.Perdure.CDeserializer

-- | When we write Words and read them back on a platform with a different Word size, the array length changes,
-- and each Word64 is replaced by two consecutive Words32 (least-significant first), (the reverse substitution is applied
-- when going from Word32 to Word64, with an implicit 0 at the end when the size is odd)

instance (ArrayRef r32, ArrayRef r64, ArrayRefElem r32 ~ Word32, ArrayRefElem r64 ~ Word64) => ArrayRef (WordArrayRef r32 r64) where
  type ArrayRefElem (WordArrayRef r32 r64) = Word
  writeArrayRef l b = onWordConv
                      (Word32ArrayRef <$> writeArrayRef l (apply wordConv1 b)) 
                      (Word64ArrayRef <$> writeArrayRef l (apply wordConv1 b))
  derefArrayRef f (Word32ArrayRef r) = fmap deserInput <$> derefPinnedArrayRef f r
  derefArrayRef f (Word64ArrayRef r) = fmap deserInput <$> derefPinnedArrayRef f r
  arrayRefAddr (Word32ArrayRef r) = arrayRefAddr r
  arrayRefAddr (Word64ArrayRef r) = arrayRefAddr r
  arrayRefSize (Word32ArrayRef r) = arrayRefSize r
  arrayRefSize (Word64ArrayRef r) = arrayRefSize r

derefPinnedArrayRef :: (ArrayRef r, StoreFile f) => f -> r (StoreRef f) -> IO (Maybe (ArrayRange (PrimArray Pinned (ArrayRefElem r))))
derefPinnedArrayRef = derefArrayRef

