{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving #-}

module Database.Perdure.SizeRef (
  SizeRef
  ) where

import Data.Bits
import Cgm.Data.Word
import Cgm.Data.Len
import Database.Perdure.Persistent
import Cgm.Data.Functor.Sum
import Database.Perdure.DRef
import Database.Perdure.Allocator
import Database.Perdure.StoreFile
import Cgm.Data.Nat
import Data.Dynamic

data SizeRef n a = TooSmallForRef !a  | LargeEnoughForRef !(DRef a) deriving Typeable
deriving instance Show a => Show (SizeRef n a)

instance Persistent1 (SizeRef n) where persister1 = structureMap $ persister |. persister1
instance Nat n => RefPersistent (SizeRef n) where
  refPersister = RefView (either (TooSmallForRef . deref) LargeEnoughForRef . getSum) $ 
                 (SizeRefPersister (refineLen l)) where
    l :: Len Word8 Word
    l = unsafeLen $ 1 `shiftL` (at :: At n) intOfNat
  -- The use of the SizeRef constructor above instead of struct is required because the Structure typeclass seems
  -- only able to dispatch on monotypes
  -- Also the type annotation is necessary, and it seems to be because we are using the InjectionA typeclass, with no functional dependencies between
  -- the related types. 
instance Deref (SizeRef n) where derefIO = either return derefIO . structure

deriveStructured ''SizeRef