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

module Database.Perdure.WordNArrayRef (
  WordNArrayRef(..),
  WordNValidator,
  module Database.Perdure.ArrayRef
  ) where

import Prelude()
import Cgm.Prelude
import Database.Perdure.ArrayRef
import Database.Perdure.Persistent
import Cgm.Data.Word
import Cgm.System.Endian
import Database.Perdure.WValidator

class (Validator v, Persistent v, LgMultiple Word64 (ValidatedElem v), Endian (ValidatedElem v), LgMultiple (ValidatedElem v) Word8) => WordNValidator v
instance WordNValidator W32Validator
instance WordNValidator W64Validator

instance WordNValidator v => ArrayRef (WordNArrayRef v) where
  type ArrayRefElem (WordNArrayRef v) = ValidatedElem v
  writeArrayRef l b = do
    let (v, buf) = mkValidationInput b
    r <- allocWrite l platformWordEndianness buf
    return $ WordNArrayRef v r platformWordEndianness
  derefArrayRef f (WordNArrayRef v r e) = fmap primArrayMatchAllocation <$> await1 (storeFileRead f r e v)
  arrayRefAddr (WordNArrayRef _ r _) = refStart r

