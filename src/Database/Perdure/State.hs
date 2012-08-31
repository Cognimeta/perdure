{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections, FlexibleContexts, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}

module Database.Perdure.State(
  PState(stateLocation, stateSpace, stateRoot),
  --initStateN,
  Root(..),
  RootValues(..),
  rootAllocSize,
  initState,
  readState,
  writeState,
  --asyncWriteState,
  updateState,
  updateStateRead,
  --asyncCollectState,
  collectState,
  collectStateM,
  emptyCache,
  module Database.Perdure.Space,
  module Database.Perdure.Persistent,
  CachedFile(..),
  RootLocation(..),
  RootAddress(..),
  stateValue
  ) where

import Prelude()
import Cgm.Prelude
import Control.Concurrent
import qualified Cgm.Control.Monad.State as M
import Control.Monad.State hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Cgm.Data.Word
import Cgm.Data.Len
import Cgm.Data.List
import Database.Perdure.Persistent
import Database.Perdure.CSerializer
import Database.Perdure.Decrementer
import Database.Perdure.Incrementer
import Database.Perdure.RootValidator
import Database.Perdure.Space
import Database.Perdure.SpaceTree
import Database.Perdure.Data.MapMultiset
import Cgm.System.Endian
import Cgm.Data.Multiset as MS
import Database.Perdure.Ref
import Database.Perdure.Deref
import Database.Perdure.SpaceBook
import Cgm.Data.Typeable
import qualified Data.Cache.LRU as LRU
import Cgm.Data.Maybe
import Database.Perdure.WriteBits
import Database.Perdure.AllocCopy
import Cgm.Data.Super
import Database.Perdure.Allocator
import Database.Perdure.Rev

data CachedFile = CachedFile ReplicatedFile (MVar Cache)

-- | The RootLocation specifies where roots are written, and provides a cache.
data RootLocation = RootLocation CachedFile [RootAddress]

-- | A number which is incremented every time a state is written.
newtype StateId = StateId Word64 deriving (Ord, Eq, Show, Enum, Bounded, Persistent, Num, Real, Integral)

newtype RootAddress = RootAddress {getRootAddress :: Address} deriving (Eq, Show)
rootRef :: LgMultiple Word64 w => RootAddress -> BasicRef w
rootRef (RootAddress a) = BasicRef a $ refineLen rootAllocSize

-- | The PState represents the whole state of the database. It is needed to perform updates.
data PState a = PState {
  stateLocation :: RootLocation,
  stateSpace :: SpaceTree,
  -- ^ stateSpace is always a subset of (or the same as) the true free space as determined by scanning rootScan from rootScan's s and c
  -- This subset is maintained conservatively to speed up new allocations.
  stateRoot :: RootVersions a -- ^ The persisted data consisting of bookkeeping data and user data.
  }

stateValue :: PState a -> a
stateValue = deref . rootValue . rootScan . toOnlyRev . stateRoot

-- LATER For now a root occupies a constant space (an id and 4 DRefs). It would be preferable to
-- not use a CDRef of a but a SizeRef that would allow us to write more data in the root (keeping in mind though that we need room
-- for 2 such references, after we have deducted the space for the id and the other two DRefs).
-- | Root persisted data. The 'a' type parameter is the user persisted data type.
data Root a = Root {
  rootId :: StateId, 
  rootDecr :: Maybe (RootValues a),
  rootScan :: RootValues a
  }

type RootVersions a = Root a :> NoRev

instance (Persistent a, Typeable a) => Persistent (Root a) where persister = structureMap persister

data RootValues a = RootValues {
  rootCS :: CDRef SpaceBook, 
  rootValue :: CDRef a
  }

instance (Persistent a, Typeable a) => Persistent (RootValues a) where 
  persister = structureMap persister

-- | Writes an initial state (creates a new database).
-- Most often the passed 'a' will be a fresh unpersisted value. This is always safe.
-- However it is legal for parts of 'a' to be already persisted, but they must only use allocations within the passed SpaceTree.
-- To read the state of an existing database use readState.
initState :: (Persistent a, Typeable a) => RootLocation -> SpaceTree -> a -> IO (PState a)
initState = initStateN minBound

-- | The persisted value 'a' must only use memory contained in 'SpaceTree'.
initStateN :: (Persistent a, Typeable a) => StateId -> RootLocation -> SpaceTree -> a -> IO (PState a)
initStateN i l s a = 
  let sb = SpaceBook MS.emptySet s
      s' = bookSpace $ incr persister a sb -- Before we can write anything else, we need to know what memory is actually free.
  in await0 $ writeRootSimple l s' $ Current $ Root i Nothing $ RootValues (ref sb) (ref a) 

-- | Reads the state of an existing database. It only reads the root, and the rest is lazy loaded.
-- The RootLocation must match the one use when writing. On failure it returns Nothing.
readState :: (Persistent a, Typeable a) => RootLocation -> IO (Maybe (PState a))
readState l@(RootLocation f rootAddrs) = 
  fmap (\r -> let rs = rootScan $ toOnlyRev r in PState l (bookSpace $ incr persister rs $ deref $ rootCS rs) r) . 
  maybeMaximumBy (comparing $ rootId . toOnlyRev) . catMaybes <$> sequence (readRoot f <$> rootAddrs)

-- | Create an empty cache of the specified size (number of dereferenced DRefs).
-- Note that eventually we would like a cache with a size measured in bytes, for a better prediction of memory consumption.
emptyCache :: Integer -> Cache
emptyCache sz = LRU.fromList (Just sz) []

-- | We reserve the option of growing roots to 1MB, so use this as a minimum distance between the various RootAddress in RootLocation
rootAllocSize :: Len Word64 Word32
rootAllocSize = coarsenLen (unsafeLen (1024 * 1024) :: Len Word8 Word32)

-- For now, asyncWriteState should be used synchronously (with await0) because we have no flow control on the StoreFile.
asyncWriteState :: (Persistent a, Typeable a) => a -> PState a -> IO () -> IO (PState a)
asyncWriteState a rs = 
  (if rootId (toOnlyRev $ stateRoot rs) `mod` collectFrequency == 0 then \w k -> collectState rs >>= ($ k) . w else ($ rs)) $
  \(PState l s rvers) -> case toOnlyRev rvers of (Root i d (RootValues rcs _)) ->
                                                   writeRootSimple l s $ Current $ Root (succ i) d $ RootValues rcs $ ref a
  
collectFrequency :: StateId
collectFrequency = 1000

writeRootSimple :: forall a. (Persistent a, Typeable a) => RootLocation -> SpaceTree -> RootVersions a -> IO () -> IO (PState a)
writeRootSimple l@(RootLocation f _) s r done = 
  fmap (\(r', _ :: MapMultiset Address, s') -> PState l s' r') $ -- We may want to rewrite 'write' so it does not produce counts, and drop the Multiset c context
  writeRoot (rootId $ toOnlyRev r) l done $ write f s r

-- | Takes the current state and the new value to be written, and writes and returns a new state. Writing is strict so make sure you do
-- not have cycles in the value to be written. After writing, you should no longer use the value you passed in, but instead use the
-- equivalent value present in the in the returned state. That new equivalent value knows where it is stored and will be lazily loadable.
-- The value just written will be partially or totally in the cache. IMPORTANT: This call overwrites the value that was in the state
-- passed as input, so you should not use it after this call returns. However it is safe for this call to use it implicitly, because often
-- the new value will be a function of the old one, and the strict write process will force parts of the old value to be read. If
-- by accident you do use a value which was overwritten, its digests will be incorrect (with very high probability) and deref will return error.
-- This calls collectState implicity once every 1000 calls. We will make this optional in future revisions.
writeState :: (Persistent a, Typeable a) => a -> PState a  -> IO (PState a)
writeState a r = await0 $ asyncWriteState a r

-- | Writes a new state if the passed state change requires it. The StateT monad used here is like the usual StateT monad but
-- it has an additional 'unchanged' case which allow us to avoid needless writes.
updateState :: (Persistent a, Typeable a, MonadIO m) => M.StateT a m b -> M.StateT (PState a) m b
updateState = updateStateRead . M.mapStateT lift

-- | Like updateState but the updater has access to the input PState throught an additional ReaderT
updateStateRead :: (Persistent a, Typeable a, MonadIO m) => M.StateT a (ReaderT (PState a) m) b -> M.StateT (PState a) m b
updateStateRead (M.StateT u) = 
  M.StateT $ \t -> runReaderT (u (deref $ rootValue $ rootScan $ toOnlyRev $ stateRoot t)) t 
                   >>= \(b, ma') -> liftM (b,) $ maybe (return Nothing) (liftM Just . liftIO . flip writeState t) ma'

-- collectState could overlap some of its work with normal writes. It could increment the latest state it sees when it starts, then decrement the old
-- state, and finally do an atomic update of the state, rereading it to integreate any new states which have been added in the mean time. But we
-- leave that for later.
asyncCollectState :: (Persistent a, Typeable a) => PState a -> IO () -> IO (PState a)
asyncCollectState (PState l _ rvers) done = -- we ignore s
  let rcs'@(SpaceBook _ s') = incr persister d' $ deref rcs
      (Root i md d'@(RootValues rcs ra)) = toOnlyRev rvers
  in writeRootSimple l s' (Current $ Root (succ i) (Just d') $ RootValues (ref $ decr persister md rcs') ra) done
     -- we write s'' but we do not use it, it will be used on next collectState
     -- s'' contains memory we just freed. We cannot reuse it immediately in writing the new state in case we crash in the process.

-- | Collects the garbage accumulated by the calls to writeState. Uses reference counting: first does an increment pass on
-- the current value, and then does a decrement pass on the value that was present at the last collection. Only new allocations are
-- scanned in the increment pass, not what was already allocated at the last collection. The decrement pass only traverses
-- the allocations that need to be deallocated.
collectState :: (Persistent a, Typeable a) => PState a -> IO (PState a)
collectState r = await0 $ asyncCollectState r

collectStateM :: (Persistent a, Typeable a) => M.StateT (PState a) IO ()
collectStateM = get >>= lift . collectState >>= put

---------------

writeRoot :: StateId -> RootLocation -> IO () -> StateT (ABitSeq RealWorld) IO a -> IO a
writeRoot i (RootLocation (CachedFile f _) roots) done writer = do
  start <- stToIO mkABitSeq
  (result, end) <- runStateT writer start
  {-# SCC "barrier" #-} storeFileFullBarrier f -- If the following write completes, we want to assume that all preceeding writes completed
  ar <- allocCopyBits start end
  onWordConv (writeRoot' (apply wordConv1 ar :: PrimArray Pinned Word32)) (writeRoot' (apply wordConv1 ar :: PrimArray Pinned Word64))
  -- We would like a full barrier here, to make sure reads do not move before matching writes, there is one just before,
  -- and the precedeeing write is protected by the following sync (and will never be read)
  {-# SCC "sync" #-} storeFileSync f done -- We need to know when the transaction has become durable
  -- No barrier here, however that does not help much since we have a barrier before the preceeding write
  return result where
    writeRoot' :: forall w. (LgMultiple Word64 w, ValidationDigestWord w) => PrimArray Pinned w -> IO ()
    writeRoot' a = {-# SCC "writeRoot" #-} do
      let (RootValidator, bufs) = mkValidationInput a
      when (sum (arrayLen <$> bufs) > apply super (refineLen rootAllocSize :: Len w Word32)) $ error "Root too large."
      () <$ storeFileWrite f (getRootAddress $ genericIndex roots $ mod i $ fromIntegral $ length roots) platformWordEndianness bufs

-- No need for barrier after write root. If the root writing is delayed
-- past later writes which are starting to write the next state, we should already be garanteed that the new writes are not
-- overwriting the allocations of the previous state, otherwise the system would not be resumable after a crash.

write :: (Multiset c, Persistent a', Space ls) => 
         CachedFile -> ls -> a' -> StateT (ABitSeq RealWorld) IO (a', c Address, ls)
write (CachedFile f c) ls a' = {-# SCC "serialize" #-} StateT $ \s -> do
  cd <- newMVar emptySet
  lv <- newMVar ls
  cSer persister (c, StateAllocator f lv, Just cd) (\a'' s' -> readMVar lv >>= \ls' -> readMVar cd >>= \u -> return ((a'', u, ls'), s')) a' s

readRoot :: forall a. (Persistent a, Typeable a) => CachedFile -> RootAddress -> IO (Maybe (RootVersions a))
readRoot (CachedFile f c) rootAddr =
  fmap (deserializeFromFullArray $ apply cDeser persister $ DeserializerContext f c) <$> 
  foldM (\result next -> maybe next (return . Just) result) Nothing readRootDataWE where
    readRootData :: forall w. ValidationDigestWord w => Endianness -> Tagged w (IO (Maybe (ArrayRange (PrimArray Free Word))))
    readRootData e = tag $ fmap (fmap deserInput) $ await1 $ 
                     storeFileRead f (rootRef rootAddr) e (RootValidator :: RootValidator w)
    readRootDataW e = onWordConv id reverse [(at :: At Word32) $ readRootData e, (at :: At Word64) $ readRootData e]
    readRootDataWE = concat $ transpose $ readRootDataW <$> [platformWordEndianness, reverseEndianness platformWordEndianness]

---------------

data StateAllocator a = StateAllocator ReplicatedFile (MVar a)

instance Space a => Allocator (StateAllocator a) where
  allocatorStoreFile (StateAllocator f _) = f
  alloc (StateAllocator _ var) len = modifyMVar var $ \a -> do
    let spn = requireSpan a
    let a' = removeSpan spn a
    --putStrLn $ "Alloc " ++ show size ++ "@" ++ show (onSortedPair const spn) ++ "  => free: " ++ (show a') --findSpan 0 a'
    return (a', onSortedPair (\start _ -> unsafeLen start) spn) where
      requireSpan = onSortedPair (\start _ -> unsafeSortedPair start (start + getLen len)) . 
                    fromMaybe (error "Out of storage space") . listHead . 
                    findSpan (getLen len)

deriveStructured ''Root
deriveStructured ''RootValues
