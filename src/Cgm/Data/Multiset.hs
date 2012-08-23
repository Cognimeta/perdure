{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Cgm.Data.Multiset (
  Multiset(..)
  ) where

import Prelude()
import Cgm.Prelude
import Cgm.Data.Maybe

class Multiset a where
  emptySet :: a e
  insert :: Ord e => e -> a e -> a e
  delete :: Ord e => e -> a e -> Maybe (a e) -- ^ Returns Nothing when the elem was absent

-- | Inefficient instance. May be useful as a temporary solution.
instance Multiset [] where
  emptySet = []
  insert = (:)
  delete e a = case break (== e) a of (before, rest) -> (before ++ tail rest) `justIf` not (null rest)

