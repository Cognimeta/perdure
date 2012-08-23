{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeOperators #-}

module Cgm.Data.Super (
    Super(..),
    up,
    superSInc,
    narrowIntegral,
    module Cgm.Data.Maybe,
    module Cgm.Control.InFunctor
) where

import Data.Int
import Data.Word
import Data.Bits
import Control.Applicative
import Cgm.Data.Maybe
import Cgm.Control.InFunctor

-- Laws: properties of and relations on between values in a must still hold in b. For example 0 must map to 0, and Ord must be respected.
-- However some properites cannot hold, such as (== maxBound). So this law needs some elaboration.
class Super a b where
    super ::  InjectionM' a b

superSInc :: Super a b => a :>> b
superSInc = uncheckedStrictlyIncreasing (apply super)

up :: Super a b => a -> b
up = apply super

instance Super Word8 Word64 where super = integralInjectionMaxOnly
instance Super Word8 Word32 where super = integralInjectionMaxOnly
instance Super Word8 Word16 where super = integralInjectionMaxOnly
instance Super Word16 Word64 where super = integralInjectionMaxOnly
instance Super Word16 Word32 where super = integralInjectionMaxOnly
instance Super Word32 Word64 where super = integralInjectionMaxOnly

instance Super Word8 Word where super = integralInjectionMaxOnly
instance Super Word16 Word where super = integralInjectionMaxOnly
instance Super Word32 Word where super = integralInjectionMaxOnly
instance Super Word Word64 where super = integralInjectionMaxOnly

instance Super Word8 Integer where super = integralInjection 

integralInjectionMaxOnly :: forall a b. (Integral a, Integral b, Bounded a) => InjectionM' a b
integralInjectionMaxOnly = uncheckedInjectionM fromIntegral narrowIntegralMaxOnly

integralInjection :: forall a b. (Integral a, Integral b, Bounded a) => InjectionM' a b
integralInjection = uncheckedInjectionM fromIntegral narrowIntegral

{-# INLINE narrowIntegral #-}
narrowIntegral :: forall a b. (Integral a, Integral b, Bounded b) => a -> Maybe b
narrowIntegral = (fromIntegral <$>) . predJust (\a -> fromIntegral (minBound :: b) <= a && a <= fromIntegral (maxBound :: b))

narrowIntegralMaxOnly :: forall a b. (Integral a, Integral b, Bounded b) => a -> Maybe b
narrowIntegralMaxOnly = (fromIntegral <$>) . predJust (<= fromIntegral (maxBound :: b))

