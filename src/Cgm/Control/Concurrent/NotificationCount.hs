{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Control.Concurrent.NotificationCount (
  NotificationCount,
  withNotificationCount,
  notifying
  ) where

import Data.Word
import Control.Concurrent
import Control.Applicative
import Control.Arrow

-- Counts required notifications that have not yet been received. When they reach zero, registered actions are called.
type NotificationCount = MVar (Word64, [IO ()])

mkNotificationCount :: IO NotificationCount
mkNotificationCount = newMVar (0, [])

requireNotify :: NotificationCount -> IO ()
requireNotify = flip modifyMVar_ $ return . first (+ 1)

notify :: NotificationCount -> IO ()
notify = flip modifyMVar_ $ \(n, ios) -> if n == 1 then const (0,[]) <$> sequence_ ios else return (n - 1, ios)

whenFullyNotified :: IO () -> NotificationCount -> IO ()
whenFullyNotified io = flip modifyMVar_ $ \s@(n, ios) -> if n == 0 then const s <$> io else return (n, io : ios)

withNotificationCount :: (NotificationCount -> IO a) -> IO () -> IO a
withNotificationCount f k = mkNotificationCount >>= \n -> f n >>= (<$ whenFullyNotified k n)

notifying :: NotificationCount -> (IO () -> IO a) -> IO a
notifying n kf = requireNotify n >> kf (notify n)