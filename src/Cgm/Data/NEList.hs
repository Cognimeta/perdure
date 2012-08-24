{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Data.NEList (
  NEList,
  neSingleton,
  neCons,
  onNEList,
  neAppend
  ) where

-- Non empty list

data NEList a = Single a | NECons a (NEList a) deriving Show
neSingleton = Single
neCons = NECons
onNEList :: (a -> Maybe (NEList a) -> z) -> NEList a -> z
onNEList f l = case l of {Single a -> f a Nothing; NECons a r -> f a (Just r)}
neAppend l1 l2 = onNEList (\a1 r1 -> neCons a1 $ maybe id neAppend r1 l2) l1
