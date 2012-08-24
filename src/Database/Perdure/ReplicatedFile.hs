{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies #-}

module Database.Perdure.ReplicatedFile (
  ReplicatedFile(..)
  ) where

import Control.Applicative
import Database.Perdure.StoreFile
import Database.Perdure.LocalStoreFile
import Cgm.Control.Concurrent.NotificationCount
import Cgm.Data.Word

newtype ReplicatedFile = ReplicatedFile [LocalStoreFile]

instance StoreFile ReplicatedFile where
  type StoreRef ReplicatedFile = BasicRef
  storeFileWrite (ReplicatedFile fs) addr e bufs = do
    sequence_ $ fmap (\f -> storeFileWrite1 f addr e bufs) fs
    return (BasicRef addr $ narrowBufsLen bufs)
  storeFileRead (ReplicatedFile fs) (BasicRef addr size) e v k =
    foldr (\f retry -> await1 (storeFileRead1 f addr size e v) >>= maybe retry (k . Just)) (k Nothing) fs 
  
instance SyncableStoreFile ReplicatedFile where
  storeFileSync (ReplicatedFile fs) = withNotificationCount $ \n -> sequence_ $ (notifying n . storeFileSync) <$> fs
  storeFileFullBarrier (ReplicatedFile fs) = sequence_ $ storeFileFullBarrier <$> fs

-- TODO see if we can have the user threads perform the digest checks

