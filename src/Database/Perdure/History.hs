{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Database.Perdure.History (
  History,
  initial,
  current,
  insert,
  updateHistory,
  updateHistoryM
  ) where

import Data.Bits
import Database.Perdure.Persistent
import Control.Arrow
import Control.Applicative
import qualified Control.Monad.State as Std
import Cgm.Control.Monad.State
import Control.Monad
import Database.Perdure.Ref
import Database.Perdure.Deref
import Data.Dynamic


newtype Queue a = Queue (CDRef [a]) deriving (Show, Typeable)
singletonQueue :: a -> Queue a
singletonQueue a = Queue $ ref [a]

queueInsert :: a -> Queue a -> (Queue a, Maybe a)
queueInsert a (Queue as) = first (Queue . ref) $ trim (a : deref as) where
  trim l = 
    if length l <= maxQueueLength 
    then (l, Nothing)
    else case reverse $ take (maxQueueLength + 1) l of -- the 'take' drops elements when we decide to reduce maxQueueLength
      (o : r) -> (reverse r, Just o)
      [] -> undefined

maxQueueLength :: Int
maxQueueLength = 2

-- | The History type is used as the state type so as to keep some snapshots of the past, in case data is lost due to a programming error
-- by the application developer. It is an homogenous collection so the argument type has to take care of versionning.
-- To avoid needless reserialization of the past states, the argument type should be a Ref type.
-- 
-- We keep the last n samples inserted, then n samples keeping one out of every two, 
-- then n samples keeping one out of every four... Currently n is hard coded to 2.
data History a = History Integer [Queue a] deriving (Show, Typeable)

-- As an example:
-- *Database.Perdure.History> foldr insert empty [0..999] -- if n were 8
-- History 1000 [Queue [0,1,2,3,4,5,6,7],Queue [9,11,13,15,17,19,21,23],Queue [27,31,35,39,43,47,51,55],Queue [63,71,79,87,95,103,111
-- ,119],Queue [127,143,159,175,191,207,223,239],Queue [255,287,319,351,383,415,447,479],Queue [543,607,671,735,799,863,927,991]]
-- If the sampling interval is 1 second, and maxQueueLength is 8, we get roughly 8 * log3(running-time/k)
-- For 3 years that's roughly 8*17 = 136 snapshots

initial :: a -> History a
initial = History 1 . pure . singletonQueue

-- | Changes a transformation on 'a' into a transformation on 'History a'. Adds a new state into the 'History'.
updateHistory :: Monad m => Std.StateT a m b -> Std.StateT (History a) m b
updateHistory = Std.StateT . (\u h -> liftM (second $ flip insert h) (u $ current h) )  . Std.runStateT

-- | Changes a transformation on 'a' into a transformation on 'History a'. Adds a new state into the 'History',
-- unless the state has not changed. Uses 'Cgm.Control.Monad.State'.
updateHistoryM :: Monad m => StateT a m b -> StateT (History a) m b
updateHistoryM = StateT . (\u h -> liftM (second $ fmap $ flip insert h) (u $ current h) )  . runStateT

-- | The history is never empty, so it is safe to get the current sample.
current :: History a -> a
current (History _ q') = case q' of
  (Queue q : _) -> head $ deref q
  [] -> undefined

--Used when we want to flush the history
--insert :: a -> History a -> History a
--insert a _ = initial a

-- | Add a newer state to the history
insert :: a -> History a -> History a
insert a (History n qs) = History (n+1) $ ins a qs n where
  ins o [] _ = [singletonQueue o]
  ins o (q:qr) m = let (q', mo') = queueInsert o q in q' : maybe qr (\o' -> if testBit m 0 then qr else ins o' qr (m `shiftR` 1)) mo'
                                                      -- we could use a probability instead of this alternating pattern, but this is easier to implement

instance (Typeable a, Persistent a) => Persistent (History a) where persister = structureMap persister                            
instance (Typeable a, Persistent a) => Persistent (Queue a) where persister = structureMap persister
deriveStructured ''History
deriveStructured ''Queue
