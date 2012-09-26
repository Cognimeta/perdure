{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Database.Perdure.Cache1 (
  Cache(..),
  Entry(..),
  empty,
  lookup,
  insert,
  delete
  ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Monad.Random
import Control.Applicative hiding (empty)
import Control.Arrow
import Data.List(minimumBy)
import Data.Ord

data Cache k a = Cache {contents :: Map.Map k (a, Integer), step :: Integer, size :: Integer, free :: Integer}

class Entry a where entrySize :: a -> Integer

empty :: Integer -> Cache k a
empty sz = Cache Map.empty 0 sz sz

lookup :: Ord k => k -> Cache k a -> Maybe (a, Cache k a)
lookup k (Cache c p s f) = fmap (\(a1, _) -> (a1, Cache (Map.insert k (a1, p) c) (p + 1) s f)) $ Map.lookup k c
  
insert :: (RandomGen g, Ord k, Entry a) => k -> a -> Cache k a -> Rand g (Cache k a)
insert k v cache =
  let es = entrySize v
      insert' (Cache c p s f) = Cache (Map.insert k (v, p) c) (p + 1) s (f - es)
  in (if es <= size cache
      then fmap insert' $ requireFree es cache
      else return cache) where
    requireFree :: (RandomGen g, Ord k, Entry a) => Integer -> Cache k a -> Rand g (Cache k a)
    requireFree n ca = if free ca > n then return ca else flip deleteExisting ca <$> oldestOfSample ca
    oldestOfSample :: RandomGen g => Cache k a -> Rand g (k, a)
    oldestOfSample (Cache c _ _ _) =
      fmap ((fst &&& fst . snd) . minimumBy (comparing $ snd . snd)) $
      sequence $ replicate 10 $ fmap (flip Map.elemAt c) $ getRandomR (0, Map.size c - 1)
    
deleteExisting :: (Ord k, Entry a) => (k, a) -> Cache k a -> Cache k a
deleteExisting (k, a) (Cache c p s f) = Cache (Map.delete k c) p s $ f + entrySize a

delete :: (Ord k, Entry a) => k -> Cache k a -> Cache k a
delete k (Cache c p s f) =
  let (mv, c') = Map.updateLookupWithKey (const $ const Nothing) k c
  in Cache c' p s $ maybe id ((+) . entrySize . fst) mv f
