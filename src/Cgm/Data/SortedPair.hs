{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Data.SortedPair (
  SortedPair,
  onSortedPair,
  sortedPair,
  unsafeSortedPair
  ) where

import Cgm.Control.InFunctor

-- SortedPair
-- The two values cannot be equal. The second is strictly greater than the first.
  
data SortedPair a = SortedPair a a deriving Show
onSortedPair :: (a -> a -> z) -> SortedPair a -> z
onSortedPair f (SortedPair a1 a2) = f a1 a2
sortedPair :: Ord a => a -> a -> SortedPair a
sortedPair a1 a2 = if a1 <= a2 then SortedPair a1 a2 else SortedPair a2 a1
unsafeSortedPair = SortedPair

instance StrictlyIncreasingFunctor SortedPair where sincmap f (SortedPair a1 a2) = SortedPair (f $* a1) (f $* a2)

