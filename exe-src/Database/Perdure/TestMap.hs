{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Database.Perdure.TestMap(
  testMap
  ) where

import Prelude hiding (null, lookup)
import Database.Perdure.Data.Map
import Test.QuickCheck
import Data.List(foldl')

testMap :: [String] -> IO ()
testMap _ = 
  (quickCheckWith (Args Nothing n n n True) $ insertList [1 .. k] empty == insertList (reverse [1 .. k]) empty) >> 
  (quickCheckWith (Args Nothing n n n True) $ deleteList [1 .. k] (insertList (reverse [1 .. k]) $ insertList [1 .. k] empty) == empty) where
  insertList l m = foldl' (\z v -> insert v v z) m l
  deleteList l m = foldl' (flip delete) m l
  n = 5
  k = n * 10000
