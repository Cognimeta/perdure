{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}


module Database.Perdure.Ref (
  Ref(..),
  ref,
  refLens
  ) where

import System.IO.Unsafe
import Database.Perdure.Deref
import Control.Lens
import Database.Perdure.Persistent
import Control.Applicative

class Deref r => Ref r where
  refIO :: a -> IO (r a)

{-# NOINLINE ref #-}
ref :: Ref r => a -> r a
ref = unsafePerformIO . refIO

refLens :: Ref r => Lens' (r a) a
refLens = lens deref $ const ref

instance Ref r => Ref (IRef r) where refIO = (IRef <$>) . refIO
instance Ref Ref0 where refIO = return . Ref0
instance Deref r => Ref (CRef r) where refIO = return . ToRef
instance Deref r => Functor (CRef r) where fmap f = ref . f . deref
