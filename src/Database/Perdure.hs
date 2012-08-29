{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Perdure(
  module Database.Perdure.Persistent,
  defaultRootLocation,
  newCachedFile,
  PVar,
  openPVar,
  createPVar,
  updatePVar,
  updateInspectPVar
  ) where

import Prelude()
import Cgm.Prelude
import Database.Perdure.State
import Database.Perdure.Persistent
import Cgm.Control.Monad.State as M
import Cgm.Data.Typeable
import Control.Monad hiding (sequence)
import Control.Monad.Trans
import Control.Monad.Reader hiding (sequence)
import Control.Applicative
import Cgm.Control.Concurrent.MVar
import Cgm.Data.Super
import Data.Word

-- | Wraps a ReplicatedFile with cache of a given size (number of dereferenced DRefs)
newCachedFile :: Integer -> ReplicatedFile -> IO CachedFile
newCachedFile sz f = CachedFile f <$> newMVar (emptyCache sz)

-- | At the moment this is the only way to create a rootLocation.
-- The root of the database will be located in one of two reserved locations at the start of the specified files.
defaultRootLocation :: CachedFile -> RootLocation
defaultRootLocation f = RootLocation f [RootAddress 0, RootAddress $ apply super rootAllocSize]

-- | Represents a persisted database. Contains a (ram-only) lock to sequence multithreaded operations,
-- so only one 'PVar' must be created per 'RootLocation'.
newtype PVar s = PVar (MVar (PState s))

-- | Creates a PVar with the specified initial state. Writes at the specified location, using the given maximum usable space (in bytes).
createPVar :: (Typeable s, Persistent s) => s -> Word64 -> RootLocation -> IO (PVar s)
createPVar s sz l = initState l (addSpan (sortedPair roots sz') emptySpace) s >>= fmap PVar . newMVar where
  roots = 2 * apply super (getLen rootAllocSize)
  sz' =
    if sz <= roots
    then error $ "createPVar needs a max size of at least " ++ show roots ++ " bytes"
    else getLen (coarsenLen (unsafeLen (sz - roots) :: Len Word8 Word64) :: Len Word64 Word64)

-- | Attempts to open a PVar by reading at the given 'RootLocation'. Do not open the same location multiple times, share
-- the PVar instead.
openPVar :: (Typeable s, Persistent s) => RootLocation -> IO (Maybe (PVar s))
openPVar l = readState l >>= sequence . fmap (fmap PVar . newMVar)

-- | Persist a state change
updatePVar :: (Typeable s, Persistent s) => PVar s -> M.StateT s IO a -> IO a
updatePVar (PVar v) t = stateTModifyMVar v $ toStandardState $ updateState t

-- | This function allows read access to the bookkeeping structures of the database. The 'PState' type is subject to change.
updateInspectPVar :: (Typeable s, Persistent s) => PVar s -> M.StateT s (ReaderT (PState s) IO) a -> IO a
updateInspectPVar (PVar v) t = stateTModifyMVar v $ toStandardState $ updateStateRead t
