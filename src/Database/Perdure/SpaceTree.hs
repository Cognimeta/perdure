{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}

module Database.Perdure.SpaceTree(
  SpaceTree
  ) where

import Prelude()
import Cgm.Prelude
import Data.Word
import Cgm.Data.Maybe
import Cgm.Control.Monad.State
import Database.Perdure.Space
import Database.Perdure.Data.Map
import Database.Perdure.Persistent
import Debug.Trace
import Data.Typeable

newtype SpaceTree = SpaceTree (Map Word64 Bool) deriving Typeable
instance Space SpaceTree where
  emptySpace = SpaceTree Database.Perdure.Data.Map.empty
  removeSpan = onSortedPair (\b e -> spaceTreeIncr e . spaceTreeDecr b)
  addSpan = onSortedPair (\b e -> spaceTreeIncr b . spaceTreeDecr e)
  findSpan sz (SpaceTree m) = filter (onSortedPair $ \start end -> end - start > sz) $
    let
      start = 0 -- TODO pass that in as a parameter
      mkSpans [] = []
      mkSpans (k : []) = error "bad SpaceTree"
      mkSpans (k0 : k1 : ks) = unsafeSortedPair k0 k1 : mkSpans ks
    in mkSpans $
       (\(b, ks) -> bool (start : ks) ks b) $ -- When bool is false, prepend 'start' (consider that the span possibly overlapping 'start' actually starts at 'start')
       ($ (True, [])) $
       fromMaybe (const (True, [])) $
       scan (\k -> bool (\_ -> id) (\b (_, ks) -> (b, k : ks)) $ k > start) 
       (\k l r -> bool (l . r) r $ k <= start) m
    -- scan's z is of type (Bool, [Address]) -> (Bool, [Address]) where the Bool is the value of the first key in the list (undefined when empty). 
    -- Only keys > a are kept. The function prepends address, and only checks the input Bool when it has nothing to prepend.
    -- relies on the invariant where values alternate between True and False as scan keys in ascending or descending order
  isFreeSpace a (SpaceTree m) = fromMaybe False $ fromMaybe Nothing $ 
                                scan (\k -> bool (const Nothing) Just $ k <= a) (\k -> bool (\l r -> maybe l Just r) const $ k >= a) m
    -- scan's z is of type Maybe Bool and is Just the value associated to the largest key <= a, or Nothing there is no such key, cool use of lazyness

spaceTreeIncr :: Word64 -> SpaceTree -> SpaceTree
spaceTreeIncr a (SpaceTree m) = 
  SpaceTree $ fromMaybe m $ snd $ runState (updateM a $ get >>= maybe (put (Just True)) (bool (put Nothing) (error $ "spaceTreeIncr " ++ show a))) m
spaceTreeDecr :: Word64 -> SpaceTree -> SpaceTree
spaceTreeDecr a (SpaceTree m) = 
  SpaceTree $ fromMaybe m $ snd $ runState (updateM a $ get >>= maybe (put (Just False)) (bool (error $ "spaceTreeDecr " ++ show a) (put Nothing))) m

instance Persistent SpaceTree where persister = structureMap persister

deriveStructured ''SpaceTree
