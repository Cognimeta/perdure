{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleContexts, TupleSections, RankNTypes, ScopedTypeVariables, DeriveDataTypeable #-}

module Database.Perdure.CRef (
  CRef(..),
  onCRef
  ) where

import Control.Concurrent
import Control.Applicative
import System.IO.Unsafe
import Cgm.Control.Combinators
import Data.Dynamic
import Cgm.Data.Typeable
import Database.Perdure.Package


data CRef r a = Refed !(r a) | ToRef !a
instance (Typeable1 r, Typeable a) => Typeable (CRef r a) where 
  typeOf _ = mkTyCon3 perdurePackage "Database.Perdure.CRef" "CRef" `mkTyConApp` [typeOf1 (undefined :: r ()), typeOf (undefined :: a)]
                                                                                                                                                                    

{-# INLINE onCRef #-}
onCRef :: (r a -> b) -> (a -> b) -> CRef r a -> b
onCRef refed toRef st = case st of {Refed ss -> refed ss; ToRef a -> toRef a}
