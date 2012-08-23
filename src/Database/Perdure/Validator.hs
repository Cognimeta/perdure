{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, TupleSections #-}

module Database.Perdure.Validator (
  Validator(..),
  Validator0(..),
  module Cgm.Data.Array  
  ) where

import Prelude ()
import Cgm.Prelude
import Cgm.Data.Word
import Cgm.Data.Array

-- Although most hash functions are defined on strings of byte as a convention, they often start by converting the plaintext to strings
-- of 32 or 64 bit words. They may do the reverse with the output. We will restrict ourselves to such hash functions (stripped of their
-- intial and final byte conversions) so that we may apply them with equal efficiency, and identical results, on either big-endian or
-- little-endian platforms. The buffers we hash are made up of 32bit or 64bit words. This also allows for a clean design where byte conversion
-- (when needed due to the interoperation of systems of different endianness) occurs after hashing when writing, and before
-- hashing when reading. We do not have readily available implementations of the hash functions with the byte conversion layer stripped, so
-- we will use a (64-bit) hash function with a little endian byte-conversion layer (such as tiger of skein-512 from 
-- http://hackage.haskell.org/package/cryptohash-0.6.2), and add an additional byte-swap layers when we are on big-endian machines.
-- When we have time we can optimize big-endian machines by reworking the code of the library. For 32 bit platforms we will
-- use a 32 bit optimized hash algorithm, with a digest of the same size. When reading we will need to know both the endianness
-- in which the data was written, but also the word size (and thus the hash algorithm) of the written data.

class Validator v where
  type ValidatedElem v
  mkValidationInput :: PrimArray Pinned (ValidatedElem v) -> (v, [PrimArray Pinned (ValidatedElem v)]) -- TODO generalize to ArrayRange
  validate :: Prim (ValidatedElem v) => v -> ArrayRange (PrimArray Pinned (ValidatedElem v)) -> Maybe (ArrayRange (PrimArray Pinned (ValidatedElem v)))

data Validator0 w = Validator0
instance Validator (Validator0 w) where
  type ValidatedElem (Validator0 w) = w
  mkValidationInput = (Validator0, ) . pure
  validate Validator0 = Just
