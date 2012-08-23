{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections, FlexibleContexts, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Database.Perdure.State(
  RootState(stateLocation, stateSpace, stateRoot),
  --unsafeRootState,
  Root(..),
  RootValues(..),
  rootAllocSize,
  initState,
  readState,
  writeState,
  --asyncWriteState,
  updateState,
  --asyncCollectState,
  collectState,
  collectStateM,
  emptyCache,
  module Database.Perdure.Space.Space,
  module Database.Perdure.Persistent,
  No,
  no,
  May(..),
  StateLocation(..),
  RootAddress(..),
  stateValue
  ) where

import Prelude()
import Cgm.Prelude
import Debug.Trace
import Control.Concurrent
import qualified Cgm.Control.Monad.State as M
import Control.Monad.State hiding (sequence)
import Control.Monad.Reader hiding (sequence)
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Ord
import Data.Functor.Constant
import Data.Functor.Identity
import Cgm.System.Mem.Alloc
import Cgm.Data.Word
import Cgm.Data.Len
import Cgm.Data.List
import Database.Perdure.Persistent
import Database.Perdure.CSerializer
import Database.Perdure.Decrementer
import Database.Perdure.Incrementer
import Database.Perdure.RootValidator
import Database.Perdure.SizeRef
import Database.Perdure.Space.Space
import Database.Perdure.Count.Count
import Cgm.System.Endian
import Cgm.Control.Combinators
import Cgm.Control.Concurrent.MVar
import Cgm.Control.Concurrent.Await
import Cgm.Data.Monoid
import Control.Arrow
import Cgm.Data.Multiset as MS
import Debug.Trace
import Database.Perdure.Ref
import Database.Perdure.SpaceBook
import Database.Perdure.Package
import Cgm.Data.Typeable
import qualified Data.Cache.LRU as LRU
import Cgm.Data.Maybe
import Database.Perdure.WriteBits
import Database.Perdure.AllocCopy
import Cgm.Data.Super
import Database.Perdure.Allocator

moduleName :: String
moduleName = "Database.Perdure.State"

-- | The StateLocation specifies where roots are written, and provides a cache.
data StateLocation = StateLocation WriteStoreFile (MVar Cache) [RootAddress]

-- | A number which is incremented every time a state is written.
newtype StateId = StateId Word64 deriving (Ord, Eq, Show, Enum, Bounded, Persistent, Num, Real, Integral)

newtype RootAddress = RootAddress {getRootAddress :: Address} deriving (Eq, Show)
rootRef :: LgMultiple Word64 w => RootAddress -> BasicRef w
rootRef (RootAddress a) = BasicRef a $ refineLen rootAllocSize

-- | The RootState represents the whole state of the database. It is needed to perform updates.
data RootState m c s a = RootState {
  stateLocation :: StateLocation,
  stateSpace :: s, -- ^ stateSpace is always a subset of (or the same as) the true free space as determined by scanning rootScan from rootScan's s and c
                            -- This subset is maintained conservatively to speed up new allocations.
  stateRoot :: m (Root (RootValues c s a) c s a) -- ^ The persisted data consisting of bookkeeping data and user data.
  }


stateValue :: Functor m => RootState m c s a -> m a
stateValue = fmap (deref . rootValue . rootScan) . stateRoot

-- LATER For now a root occupies a constant space (an id and 4 DRefs). It would be preferable to
-- not use a CDRef of a but a SizeRef that would allow us to write more data in the root (keeping in mind though that we need room
-- for 2 such references, after we have deducted the space for the id and the other two DRefs).
-- | Root persisted data. The 'a' type parameter is the user persisted data type.
data Root d c s a = Root {
  rootId :: StateId, 
  rootDecr :: Maybe d,
  rootScan :: RootValues c s a
  }

instance (Persistent a, Persistent s, Persistent (c Address), Persistent d, Typeable1 c, Typeable a, Typeable s) => 
         Persistent (Root d c s a) where persister = structureMap persister

data RootValues c s a = RootValues {
  rootCS :: CDRef (SpaceBook c s), 
  rootValue :: CDRef a
  }

instance (Persistent s, Persistent (c Address), Persistent a, Typeable1 c, Typeable a, Typeable s) => Persistent (RootValues c s a) where 
  persister = structureMap persister

-- | Create an initial state that can then be used with writeState to initialize a new database.
-- To read the state of an existing database use readState.
initState :: Space s => StateLocation -> s -> RootState No c s a
initState l s = RootState l s no

-- | Reads the state of an existing database. It only reads the root, and the rest is lazy loaded. The StateLocation must match the one use when writing. On failure it returns Nothing.
readState :: (Persistent a, Persistent (c Address), Persistent s, Space s, Multiset c, Typeable1 c, Typeable a, Typeable s) => 
             StateLocation -> IO (Maybe (RootState Identity c s a))
readState l@(StateLocation f c rootAddrs) = 
  fmap (\r -> RootState l (bookSpace $ incr persister (rootScan r) $ deref $ rootCS $ rootScan r) $ Identity r) . 
  maybeMaximumBy (comparing rootId) . catMaybes <$> sequence (readRoot f c <$> rootAddrs)

-- | Create an empty cache of the specified size (number of dereferenced DRefs). Note that eventually we would like a cache with a size measured in bytes, for a better prediction of memory consumption.
emptyCache :: Integer -> Cache
emptyCache sz = LRU.fromList (Just sz) []

-- | We reserve the option of growing roots to 1MB, so use this as a minimum distance between the various RootAddress in StateLocation
rootAllocSize :: Len Word64 Word32
rootAllocSize = coarsenLen (unsafeLen (1024 * 1024) :: Len Word8 Word32)

-- For now, asyncWriteState should be used synchronously (with await0) because we have no flow control on the StoreFile.
-- asyncWriteState returns both a Root and RootState. Root is what has been written. RootState is a
-- a function of Root which abstracts some details. RootState is sufficient to perform the next writeState.
asyncWriteState :: (Persistent a, Persistent (c Address), Multiset c, Persistent s, Space s, May m, Typeable1 c, Typeable a, Typeable s) => 
                   a -> RootState m c s a -> IO () -> IO (RootState Identity c s a)
asyncWriteState a (RootState l s mr_) = 
  maybe 
  (writeRootSimple l s $ Root minBound Nothing $ RootValues (ref (SpaceBook emptySet s)) $ ref a) 
  (asyncWriteState1 a . RootState l s . Identity)
  $ mayGet mr_
  
collectFrequency :: StateId
collectFrequency = 1000

asyncWriteState1 :: (Persistent a, Persistent (c Address), Multiset c, Persistent s, Space s, Typeable1 c, Typeable a, Typeable s) => 
                    a -> RootState Identity c s a -> IO () -> IO (RootState Identity c s a)
asyncWriteState1 a rs = 
  (if rootId (runIdentity $ stateRoot rs) `mod` collectFrequency == 0 then \w k -> collectState rs >>= ($ k) . w else ($ rs)) $
  \(RootState l s (Identity (Root i d (RootValues rcs _)))) -> writeRootSimple l s $ Root (succ i) d $ RootValues rcs $ ref a

writeRootSimple :: forall s c a. (Persistent s, Multiset c, Persistent (c Address), Persistent a, Space s, Typeable a, Typeable s, Typeable1 c) => 
                   StateLocation -> s -> Root (RootValues c s a) c s a -> IO () -> IO (RootState Identity c s a)
writeRootSimple l@(StateLocation f cache _) s r done = 
  fmap (\(r', (_ :: c Address), s') -> RootState l s' $ Identity r') $ -- We may want to rewrite 'write' so it does not produce counts, and drop the Multiset c context
  writeRoot (rootId r) l done $ write f cache s r

-- | Takes the current state and the new value to be written, and writes and returns a new state. Writing is strict so make sure you do
-- not have cycles in the value to be written. After writing, you should no longer use the value you passed in, but instead use the
-- equivalent value present in the in the returned state. That new equivalent value knows where it is stored and will be lazily loadable.
-- The value just written will be partially or totally in the cache. IMPORTANT: This call overwrites the value that was in the state
-- passed as input, so you should not use it after this call returns. However it is safe for this call to use it implicitly, because often
-- the new value will be a function of the old one, and the strict write process will force parts of the old value to be read. If
-- by accident you do use a value which was overwritten, its digests will be incorrect (with very high probability) and deref will return error.
-- This calls collectState implicity once every 1000 calls. We will make this optional in future revisions.
writeState :: (Persistent a, Persistent (c Address), Multiset c, Persistent s, Space s, May m, Typeable1 c, Typeable a, Typeable s) => 
              a -> RootState m c s a  -> IO (RootState Identity c s a)
writeState a r = await0 $ asyncWriteState a r

-- | Writes a new state if the passed state change requires it. The StateT monad used here is like the usual StateT monad but
-- it has an additional 'unchanged' case which allow us to avoid needless writes.
updateState :: (Persistent a, Persistent (c Address), Multiset c, Persistent s, Space s, Typeable1 c, Typeable a, Typeable s) => 
               M.StateT a (ReaderT (RootState Identity c s a) IO) b -> M.StateT (RootState Identity c s a) IO b
updateState (M.StateT u) = 
  M.StateT $ \t -> runReaderT (u (deref $ rootValue $ rootScan $ runIdentity $ stateRoot t)) t 
                   >>= \(b, ma') -> fmap (b,) $ maybe (return Nothing) (fmap Just . flip writeState t) ma'

-- collectState could overlap some of its work with normal writes. It could increment the latest state it sees when it starts, then decrement the old
-- state, and finally do an atomic update of the state, rereading it to integreate any new states which have been added in the mean time. But we
-- leave that for later.
asyncCollectState :: (Persistent s, Persistent (c Address), Multiset c, Persistent a, Space s, Typeable a, Typeable s, Typeable1 c) => 
                     RootState Identity c s a -> IO () -> IO (RootState Identity c s a)
asyncCollectState (RootState l@(StateLocation _ ch _) _ (Identity (Root i md d'@(RootValues rcs ra)))) done = -- we ignore s
  let rcs'@(SpaceBook _ s') = trace "starting increment"  $ incr persister d' $ deref rcs
  in writeRootSimple l s' (Root (succ i) (Just d') $ RootValues (ref $ decr persister md rcs') ra) done
     -- we write s'' but we do not use it, it will be used on next collectState
     -- s'' contains memory we just freed. We cannot reuse it immediately in writing the new state in case we crash in the process.

-- | Collects the garbage accumulated by the calls to writeState. Uses reference counting: first does an increment pass on
-- the current value, and then does a decrement pass on the value that was present at the last collection. Only new allocations are
-- scanned in the increment pass, not what was already allocated at the last collection. The decrement pass only traverses
-- the allocations that need to be deallocated.
collectState :: (Persistent s, Persistent (c Address), Multiset c, Persistent a, Space s, Typeable1 c, Typeable a, Typeable s) => 
                RootState Identity c s a -> IO (RootState Identity c s a)
collectState r = await0 $ asyncCollectState r

collectStateM :: (Persistent s, Persistent (c Address), Multiset c, Persistent a, Space s, Typeable a, Typeable s, Typeable1 c) => 
                 M.StateT (RootState Identity c s a) IO ()
collectStateM = get >>= lift . collectState >>= put

-- | The persisted value 'a' must only use memory contained in 's'.
unsafeRootState :: (Persistent s, Persistent (c Address), Persistent a, Space s, Multiset c, Typeable1 c, Typeable a, Typeable s) => 
                   StateId -> StateLocation -> s -> a -> IO (RootState Identity c s a)
unsafeRootState i l s a = 
  let sb = SpaceBook MS.emptySet s
      s' = bookSpace $ incr persister a sb -- Before we can write anything else, we need to know what memory is actually free.
  in await0 $ writeRootSimple l s' $ Root i Nothing $ RootValues (ref sb) (ref a) 

---------------

writeRoot :: StateId -> StateLocation -> IO () -> StateT (ABitSeq RealWorld) IO a -> IO a
writeRoot i (StateLocation f _ roots) done writer = do
  start <- stToIO mkABitSeq
  (result, end) <- runStateT writer start
  {-# SCC "barrier" #-} storeFileFullBarrier f -- If the following write completes, we want to assume that all preceeding writes completed
  arr <- allocCopyBits start end
  onWordConv (writeRoot' (apply wordConv1 arr :: PrimArray Pinned Word32)) (writeRoot' (apply wordConv1 arr :: PrimArray Pinned Word64))
  -- We would like a full barrier here, to make sure reads do not move before matching writes, there is one just before,
  -- and the precedeeing write is protected by the following sync (and will never be read)
  {-# SCC "sync" #-} storeFileSync f done -- We need to know when the transaction has become durable
  -- No barrier here, however that does not help much since we have a barrier before the preceeding write
  return result where
    writeRoot' :: forall w. (LgMultiple Word64 w, ValidationDigestWord w) => PrimArray Pinned w -> IO ()
    writeRoot' arr = {-# SCC "writeRoot" #-} do
      let (RootValidator, bufs) = mkValidationInput arr
      when (sum (arrayLen <$> bufs) > apply super (refineLen rootAllocSize :: Len w Word32)) $ error "Root too large."
      () <$ storeFileWrite f (getRootAddress $ genericIndex roots $ mod i $ fromIntegral $ length roots) platformWordEndianness bufs

-- No need for barrier after write root. If the root writing is delayed
-- past later writes which are starting to write the next state, we should already be garanteed that the new writes are not
-- overwriting the allocations of the previous state, otherwise the system would not be resumable after a crash.

write :: (Multiset c, Persistent a', Space ls) => 
         WriteStoreFile -> MVar Cache -> ls -> a' -> StateT (ABitSeq RealWorld) IO (a', c Address, ls)
write f c ls a' = {-# SCC "serialize" #-} StateT $ \s -> do
  cd <- newMVar emptySet
  lv <- newMVar ls
  cSer persister (c, StateAllocator f lv, Just cd) (\a'' s' -> readMVar lv >>= \ls' -> readMVar cd >>= \u -> return ((a'', u, ls'), s')) a' s

readRoot :: forall a c s d f. (Persistent a, Persistent (c Address), Persistent s, Space s, Typeable1 c, Typeable a, Typeable s) => 
            WriteStoreFile -> MVar Cache -> RootAddress -> IO (Maybe (Root (RootValues c s a) c s a))
readRoot f c rootAddr =
  (fmap $ deserializeFromFullArray $ apply cDeser persister $ DeserializerContext f c) <$> 
  foldM (\result next -> maybe next (return . Just) result) Nothing readRootDataWE where
    readRootData :: forall w. ValidationDigestWord w => Endianness -> Tagged w (IO (Maybe (ArrayRange (PrimArray Free Word))))
    readRootData e = tag $ fmap (fmap deserInput) $ await1 $ 
                     storeFileRead f (rootRef rootAddr) e (RootValidator :: RootValidator w)
    readRootDataW e = onWordConv id reverse $ [(at :: At Word32) $ readRootData e, (at :: At Word64) $ readRootData e]
    readRootDataWE = concat $ transpose $ readRootDataW <$> [platformWordEndianness, reverseEndianness platformWordEndianness]

---------------

type No = Constant ()
no = Constant ()
-- Types in May are subtypes of Maybe. There is Maybe itself. Constant () is like Maybe but where we statically know the value to be Nothing.
class Functor m => May m where mayGet :: m a -> Maybe a
instance May (Constant ()) where mayGet (Constant ()) = Nothing
instance May Maybe where mayGet = id
instance May Identity where mayGet = Just . runIdentity

---------------

data StateAllocator a = StateAllocator WriteStoreFile (MVar a)

instance Space a => Allocator (StateAllocator a) where
  allocatorStoreFile (StateAllocator f _) = f
  alloc (StateAllocator _ var) len = modifyMVar var $ \a -> do
    let span = requireSpan a
    let a' = removeSpan span a
    --putStrLn $ "Alloc " ++ show size ++ "@" ++ show (onSortedPair const span) ++ "  => free: " ++ (show a') --findSpan 0 a'
    return (a', onSortedPair (\start end -> unsafeLen start) span) where
      requireSpan = (onSortedPair $ \start _ -> unsafeSortedPair start (start + getLen len)) . 
                    ((maybe (error "Out of storage space") id) . listHead) . 
                    findSpan (getLen len)

deriveStructured ''Root
deriveStructured ''RootValues
