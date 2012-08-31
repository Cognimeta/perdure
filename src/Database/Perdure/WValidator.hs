{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, TupleSections, ScopedTypeVariables, FlexibleInstances #-}

module Database.Perdure.WValidator (
  W32Validator,
  W64Validator,
  module Database.Perdure.Validator
  ) where

import Prelude ()
import Cgm.Prelude
import Data.Word
import Database.Perdure.Digest
import Database.Perdure.Validator
import Cgm.Data.Maybe
import Cgm.Data.Structured

data W32Validator = W32Validator {-# UNPACK #-} !MD5Digest deriving (Eq, Show)
data W64Validator = W64Validator {-# UNPACK #-} !(Skein512Digest Word128) deriving (Eq, Show)

instance Validator W32Validator where
  type ValidatedElem W32Validator = Word32
  mkValidationInput b = (W32Validator $ digest b, pure b)
  validate (W32Validator h) b = b `justIf` (digest b == h)

instance Validator W64Validator where
  type ValidatedElem W64Validator = Word64
  mkValidationInput b = (W64Validator $ digest b, pure b)
  validate (W64Validator h) b = b `justIf` (digest b == h)

deriveStructured ''W32Validator
deriveStructured ''W64Validator
