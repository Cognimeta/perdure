{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

module Cgm.Data.Structured (
    Structured(..),
    struct,
    wrap,
    unwrap,
    in1,
    in2,
    structureMap,
    deriveStructured,
    module Cgm.Control.InFunctor
) where

import Cgm.Data.Structured.Derive

import Control.Arrow
import Data.Functor.Compose
import Cgm.Control.Combinators
import Cgm.Control.InFunctor

-- Similar to instant-generics but we reuse the haskell types (), (,) and Either
-- We do not make construtors explicit, nor de we introduce types Var and Rec

class Structured a where
    type Structure a
    structure :: a -> Structure a
    fromStructure :: Structure a -> a

deriveStructured ''(,,)
deriveStructured ''(,,,)
deriveStructured ''(,,,,)
deriveStructured ''(,,,,,)
deriveStructured ''[]
deriveStructured ''Maybe
deriveStructured ''Bool
deriveStructured ''Ordering

deriveStructured ''Kleisli
deriveStructured ''Compose
deriveStructured ''RevFun

{-# INLINE struct #-}
struct :: Structured a => Bijection' a (Structure a)
struct = uncheckedBijection structure fromStructure

-- TODO: decide if we replace structure and fromStructure with those
{-# INLINE unwrap #-}
unwrap :: Structured a => a -> Structure a
unwrap = structure
{-# INLINE wrap #-}
wrap :: Structured a => Structure a -> a
wrap = fromStructure

{-# INLINE in1 #-}
in1 :: (Structured a, Structured b) => (Structure a -> Structure b) -> a -> b
in1 f = wrap . f . unwrap

{-# INLINE in2 #-}
in2 :: (Structured a, Structured b, Structured c) => (Structure a -> Structure b -> Structure c) -> a -> b -> c
in2 f = wrap ./ dot2 f unwrap unwrap

{-# INLINE structureMap #-}
structureMap :: (InjectionACofunctor p, Structured a) => p (Structure a) -> p a
structureMap = (`iacomap` struct)
                                        
