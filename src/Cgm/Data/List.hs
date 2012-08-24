{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

-----------------------------------------------------------------------------
--
-- Module      :  List
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, TupleSections #-}

module Cgm.Data.List (
    testList,
    List(..),
    listHead,
    addListContext,
    foldWithContext,
    prependReverse,
    listFoldMap,
    Sizable(..),
    listStructure,
    maybeMaximumBy,
    maxBy,
    unfoldlE,
    unfoldrE
) where

--import Data.Foldable
import Data.Collections.Foldable as Foldable
import Data.Monoid
import Cgm.Control.Combinators
import Cgm.Control.InFunctor
import Control.Arrow

testList :: b -> (a -> [a] -> b) -> [a] -> b
testList empty nonEmpty list = case list of {[] -> empty; (x:xs) -> nonEmpty x xs}

class List a where
    type Listed a :: *
    onList :: b -> (Listed a -> a -> b) -> a -> b
    consList :: Listed a -> a -> a
    emptyList :: a

prependReverse :: List a => a -> a -> a
prependReverse rev end = onList end (\r rs -> prependReverse rs $ consList r end) rev

instance List [a] where
    type Listed [a] = a
    onList e _ [] = e
    onList _ c (x:xs) = c x xs
    consList = (:)
    emptyList = []

listFoldMap :: (List a, Monoid m) => (Listed a -> m) -> a -> m
listFoldMap f = onList mempty $ dot2 mappend f $ listFoldMap f

listHead :: List a => a -> Maybe (Listed a)
listHead = onList Nothing (Just ./ const)

-- For each element in a List, it makes available a reversed List of elements that come before it, and a List of those that come after it.
addListContext :: (List a, List c, Listed a ~ Listed c) => a -> [(c, Listed a, a)]
addListContext = foldWithContext (\before elem after -> ((before, elem, after) :)) []

foldWithContext :: (List a, List c, Listed a ~ Listed c) => (c -> Listed a -> a -> b -> b) -> b -> a -> b
foldWithContext f e = fold2 emptyList
    where fold2 revPrefix = onList e (\x suffix -> f revPrefix x suffix (fold2 (consList x revPrefix) suffix))

-- counts the elements, returning Nothing if at least i elements were present, or i-length if less than i elements were present
class Sizable a where countDown :: Int -> a -> Maybe Int

listStructure :: List a => Bijection' a (Maybe (Listed a, a))
listStructure = uncheckedBijection (onList Nothing $ Just ./ (,)) (maybe emptyList $ uncurry consList)

maybeMaximumBy :: Foldable l a => (a -> a -> Ordering) -> l -> Maybe a
maybeMaximumBy o = Foldable.foldr (\a -> Just . maybe a (maxBy o a)) Nothing
  
maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy o a b = case o a b of {GT -> a; _ -> b}

-- similar to unfoldl but uses Either instead of Maybe to signal the end of the list
unfoldlE :: (b -> Either c (b, a)) -> b -> (c, [a])
unfoldlE f = unfoldl' []
  where unfoldl' as = either (, as) (\(b', a) -> unfoldl' (a : as) b') . f

-- similar to unfoldr but uses Either instead of Maybe to signal the end of the list
unfoldrE :: (b -> Either c (a, b)) -> b -> ([a], c)
unfoldrE f = either ([], ) (\(a, b') -> first (a:) $ unfoldrE f b') . f
