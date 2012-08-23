{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module Cgm.Data.Division (
  Divides(..),
  divisionI,
  divI,
  undivI
  ) where

import Cgm.Control.InFunctor

-- Todo: modify so a MonotonicBijection' is returned, which make explicit the guarantee that the Ord order is preserved by the bijection.
-- (recall that we have (Ord b, Ord c) => Ord (Either b c))
class (Ord a, Ord b, Ord c) => Divides a b c where division :: Bijection' a (Either b c)

-- Uses of division are often ambiguous. When b ~ c, one may call divisionI instead.
divisionI :: Divides a b b => Bijection' a (Either b b)
divisionI = division
divI :: Divides a b b => a :>> Either b b
divI = uncheckedStrictlyIncreasing $ apply divisionI
undivI :: Divides a b b => Either b b :>> a
undivI = uncheckedStrictlyIncreasing $ retract divisionI

