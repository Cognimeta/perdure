{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Database.Perdure.Cache (
  Cache(..),
  Entry(..),
  empty,
  lookup,
  insert,
  delete
  ) where

import Prelude ()
import Cgm.Prelude hiding (empty, lookup, insert, delete)
import qualified Database.Perdure.Cache1 as Cache
import Control.Monad.Random
import Data.Dynamic
import Cgm.Data.Len
import Data.Word
import Database.Perdure.Count(Address)

data Entry = Entry {entryValue :: Dynamic, entrySize :: Len Word64 Word64}
instance Cache.Entry Entry where entrySize (Entry _ s) = fromIntegral s

newtype Cache = Cache (Cache.Cache Address Entry)

-- | Create an empty cache of the specified size in bytes of serialized data. The actual consumed size may be a few times larger
-- since heap data is usually much less compact than its serialized representation.
empty :: Integer -> Cache
empty = Cache . Cache.empty . getLen . (id :: Id (Len Word64 Integer)) . coarsenLen . (id :: Id (Len Word8 Integer)) . unsafeLen

lookup :: Address -> Cache -> Maybe (Entry, Cache)
lookup k (Cache c) = fmap (second Cache) $ Cache.lookup k c
  
insert :: RandomGen g => Address -> Entry -> Cache -> Rand g Cache
insert k v (Cache c) = fmap Cache $ Cache.insert k v c

delete :: Address -> Cache -> Cache
delete k (Cache c) = Cache $ Cache.delete k c