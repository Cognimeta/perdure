{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, Rank2Types, GADTs, TupleSections, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Database.Perdure.SingleStoreFile (
    SingleStoreFile(..)
  ) where
       
import Database.Perdure.StoreFile
import Database.Perdure.LocalStoreFile
import Cgm.Data.Word


newtype SingleStoreFile a = SingleStoreFile a deriving SyncableStoreFile
instance RawStoreFile a => StoreFile (SingleStoreFile a) where
  type StoreRef (SingleStoreFile a) = BasicRef
  storeFileWrite (SingleStoreFile f) addr e bufs = storeFileWrite1 f addr e bufs >> return (BasicRef addr $ narrowBufsLen bufs)
  storeFileRead (SingleStoreFile f) (BasicRef addr size) e v k = storeFileRead1 f addr size e v k
  
