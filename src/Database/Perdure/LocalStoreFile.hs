{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, Rank2Types, GADTs, TupleSections, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts #-}

module Database.Perdure.LocalStoreFile (
    RawStoreFile(..),
    storeFileWriteWords,
    storeFileReadWords,
    LocalStoreFile,
    withFileStoreFile,
    withRawDeviceStoreFile,
    withRawDeviceStoreFiles,
    module Database.Perdure.StoreFile,
    narrowBufsLen,
    storeFileWrite1,
    storeFileRead1
) where


import Prelude ()
import Cgm.Prelude
import Data.Typeable
import qualified Data.ByteString as B
import Control.Concurrent
import Data.Word
import qualified Data.Map as Map
import Cgm.Data.Super
import Cgm.Data.Len
import Cgm.Data.Monoid
import Cgm.Data.NEList
import Cgm.Data.Either
import Cgm.System.Endian
import Cgm.Control.Concurrent.TThread
import Cgm.Control.Concurrent.Await
import Cgm.System.Mem.Alloc
import Database.Perdure.Validator
import System.IO
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import Data.Bits
import Control.Monad.Error hiding (sequence_)
import Database.Perdure.StoreFile(SyncableStoreFile(..))
--import System.Posix.Fsync -- not needed with raw devices

class SyncableStoreFile f => RawStoreFile f where
  storeFileWriteRaw :: f -> Len Word8 Word64 -> [ArrayRange (PrimArray Pinned Word8)] -> Async Bool ()
  storeFileReadRaw :: f -> Len Word8 Word64 -> ArrayRange (STPrimArray RealWorld Pinned Word8) -> Async Bool ()

-- The passed endianness is the desired endianness of the words on the media. Here when the endianness does not
-- match the platform endianness, we do a copy. This case is not very optimized since we anticipate always writing in the
-- platform endianness.
storeFileWriteWords :: (Endian w, RawStoreFile f) => f -> Len Word8 Word64 -> Endianness -> [PrimArray Pinned w] -> Async Bool ()
storeFileWriteWords f addr e bufs = storeFileWriteRaw f addr $ fmap fullArrayRange $ fromWords bufs where
  fromWords = bool (fmap $ unsafePrimArrayCast . mapImmArray swapBytes) (fmap unsafePrimArrayCast) (e == platformWordEndianness)

-- The passed endianness must be the endianness of the words on the media
storeFileReadWords :: (LgMultiple w Word8, Endian w, RawStoreFile f) => 
                      f -> Len Word8 Word64 -> Endianness -> Len w Word32 -> Async (Maybe (PrimArray Pinned w)) ()
storeFileReadWords f addr e l k = do
    buf <- stToIO $ mkArray $ refineLen $ apply super <$> l
    ($ k) $ mapAsync (bool (return Nothing) $ Just <$> processBuf buf) $ storeFileReadRaw f addr (fullArrayRange buf) where
      processBuf buf = stToIO (toWords <$> unsafeFreezeSTPrimArray buf) -- TODO optimize by swapping bytes in-place then freezing
      toWords = bool (mapImmArray unswapBytes . unsafePrimArrayCast) unsafePrimArrayCast (e == platformWordEndianness)
  
---------------------------------------------------------------------------

-- | A file or raw device where we can persist bytes.
newtype LocalStoreFile = LocalStoreFile (MVar DevOp) 
-- TODO: review to see if this queue is acceptable
type ByteAddr = Len Word8 Word64

data PosOp = PosOp ByteAddr RWOp deriving Show
data NonPosOp = Sync (IO ()) | FullBarrier
instance Show NonPosOp where
  show (Sync _) = "Sync"
  show FullBarrier = "FullBarrier"
  
type DevOp = Either PosOp NonPosOp

data RWOp = 
  WriteOp [ArrayRange (PrimArray Pinned Word8)] (Bool -> IO ()) |
  ReadOp (ArrayRange (STPrimArray RealWorld Pinned Word8)) (Bool -> IO ())
instance Show RWOp where  
  show (WriteOp as _) = "WriteOp " ◊ show (sum $ fmap arrayLen as)
  show (ReadOp a _) = "ReadOp " ◊ show (arrayLen a)

-- TODO investigate why if we do not at force the ByteAddr in PosOp, testStatesDag ends with STM error
-- Forcing the rest is not necessary for this bug but makes sense here. We do not want the file thread to do work
-- that should have been done by the client threads. NFData is tricky to use here so we put together this
-- custom method.
forceDevOp :: DevOp -> ()
forceDevOp = either
             (\(PosOp b r) -> (b `seq`) $
                              case r of
                                WriteOp as _ -> sum (fmap arrayLen as) `seq` ()
                                ReadOp a _ -> arrayLen a `seq` ())
             (\n -> case n of
                 Sync _ -> ()
                 FullBarrier -> ())

instance RawStoreFile LocalStoreFile where
  storeFileWriteRaw f seek bufs k = queue f $ Left $ PosOp seek $ WriteOp bufs k
  storeFileReadRaw f seek buf k = queue f $ Left $ PosOp seek $ ReadOp buf k

instance SyncableStoreFile LocalStoreFile where
  storeFileSync f k = queue f $ Right $ Sync k
  storeFileFullBarrier f = queue f $ Right FullBarrier

queue :: LocalStoreFile -> DevOp -> IO () 
queue (LocalStoreFile c) op = evaluate (forceDevOp op) >> putMVar c op {- >> putStrLn ("Queued " ++ show op) -}

-- TODO rework sync mechanism. It was fine when we assumed that all completed write tasks completed successfully, but if some are
-- to remote files that time out, we want to know when all write completed successfully.


narrowBufsLen :: Endian w => [PrimArray Pinned w] -> Len w Word32
narrowBufsLen = (fromMaybe (error "Array length cannot be expressed as a Word32") . unapply super <$>) . sum . fmap arrayLen

storeFileWrite1 :: (RawStoreFile f, Endian w) => f -> Len Word64 Word64 -> Endianness -> [PrimArray Pinned w] -> IO ()
storeFileWrite1 f addr e bufs =
    storeFileWriteWords f (refineLen addr) e bufs $ bool (error "storeFileWrite failed") $ return () -- TODO pass a meaningful k

storeFileRead1 :: (RawStoreFile f, Validator v, ValidatedElem v ~ w, Endian w, LgMultiple w Word8) => 
                   f -> Len Word64 Word64 -> Len w Word32 -> Endianness -> v -> Async (Maybe (ArrayRange (PrimArray Pinned w))) ()
storeFileRead1 f addr size e v k =
    ($ k) $ mapAsync (return . (>>= {-logWhenBad .-} validate v . fullArrayRange)) $ storeFileReadWords f (refineLen addr) e size {-where
      logWhenBad Nothing = trace "Validation failed" Nothing
      logWhenBad (Just x) = Just x-}

-- After inspection of GHC.IO.FD, it seems that hFlush only flushes to the OS
-- Call chain:  
-- hFlush calls flushWriteBuffer (See http://www.haskell.org/ghc//docs/6.12.2/html/libraries/base-4.2.0.1/src/GHC-IO-Handle.html)
-- which calls writeBuf (See http://darcs.haskell.org/packages/base/GHC/IO/FD.hs)
-- which calls RawIO's write (See http://www.haskell.org/ghc/docs/6.12.3/html/libraries/base-4.2.0.2/src/GHC-IO-BufferedIO.html#flushWriteBuffer0)

-- Some links on the subject
-- http://www.scribd.com/doc/19537350/A-Wander-Through-GHCs-New-IO-Library  
-- http://lwn.net/Articles/270891/
-- http://ldn.linuxfoundation.org/article/filesystems-data-preservation-fsync-and-benchmarks-pt-1
-- http://www.linux-archive.org/device-mapper-development/422441-block-fs-replace-hardbarrier-flush-fua-take-2-a.html
-- FSync FFI is available here: http://hackage.haskell.org/packages/archive/cautious-file/0.1.5/doc/html/src/System-Posix-Fsync.html
  

--

class Show a => Schedule a where
  emptySchedule :: a
  insertOp :: PosOp -> a -> a
  nextOp :: a -> Maybe (PosOp, a)
  
-- Multimap through a Map of NEList
minViewWithKeyNE :: Ord k => Map.Map k (NEList v) -> Maybe ((k, v), Map.Map k (NEList v))
minViewWithKeyNE = (getSingle <$>) . Map.minViewWithKey where
  getSingle ((k, ne), m) = flip onNEList ne $ \single ne' -> ((k, single), maybe id (Map.insert k) ne' m)

data CLook = CLook ByteAddr (Map.Map ByteAddr (NEList PosOp)) (Map.Map ByteAddr (NEList PosOp)) deriving Show
updateLow :: (Map.Map ByteAddr (NEList PosOp) -> Map.Map ByteAddr (NEList PosOp)) -> CLook -> CLook
updateLow u (CLook c low high) = CLook c (u low) high
updateHigh :: (Map.Map ByteAddr (NEList PosOp) -> Map.Map ByteAddr (NEList PosOp)) -> CLook -> CLook
updateHigh u (CLook c low high) = CLook c low (u high)
instance Schedule CLook where
  emptySchedule = CLook 0 Map.empty Map.empty
  insertOp op@(PosOp seek _) s@(CLook c _ _) = {- (\s' -> trace ("insertOp on " ++ show s) s') $ -} bool updateLow updateHigh (seek >= c) (Map.insertWith neAppend seek (neSingleton op)) s
  nextOp (CLook _ low high) = {- trace ("nextOp on " ++ show s) $ -} useHighMin `firstJust` useLowMin where
    useLowMin = (\((pos, op), low') -> (op, CLook pos low' high)) <$> minViewWithKeyNE low
    useHighMin = (\((pos, op), high') -> (op, CLook pos low high')) <$> minViewWithKeyNE high

--
    
class Show f => RawFile f where
  fileWriteRaw :: f -> Len Word8 Word64 -> [ArrayRange (PrimArray Pinned Word8)] -> ErrorT String IO ()
  fileReadRaw :: f -> Len Word8 Word64 -> ArrayRange (STPrimArray RealWorld Pinned Word8) -> ErrorT String IO ()
  fileFlush :: f -> IO ()
  
instance RawFile Handle where
  fileWriteRaw h seek bufs = lift $ hSeekX h seek >> sequence_ (withArrayPtr (hPutBufLen h) <$> bufs) -- TODO fix error handling
  fileReadRaw h seek buf = ErrorT $ fmap (boolEither "" () . (== arrayLen buf)) $ hSeekX h seek >> withArrayPtr (hGetBufLen h) buf
  fileFlush = hFlush  -- todo fsync or fdatasync on FD so we flush to the drive (and the platter?, see comment below)

performAll :: (Schedule s, RawFile f) => f -> s -> IO ()
performAll f s = maybe (return ()) (\(op, s') -> perform f op >> performAll f s') $ nextOp s

perform :: RawFile f => f -> PosOp -> IO ()
perform f (PosOp seek rw) = {- putStrLn ("about to perform " ++ show p) >> -} case rw of 
  WriteOp bufs k -> runErrorT (fileWriteRaw f seek bufs) >>= (log' >>) . either (const $ k False) (const $ k True) where
    --log = putStrLn ("Wrote " ++ showLen (sum $ fmap arrayLen bufs) ++ " at " ++ showLen seek ++ " on " ++ show f {- ++ ": " ++ show bufs -})
    log' = return ()
    --log = putStr "w"
  ReadOp buf k -> runErrorT (fileReadRaw f seek buf) >>= (log' >>) . either (const $ k False) (const $ k True) where
    --log = putStrLn ("Read " ++ showLen (arrayLen buf) ++ " at " ++ showLen seek ++ " from " ++ show f)
    log' = return ()

process :: (Schedule s, RawFile f) => MVar DevOp -> f -> s -> IO ()
process c f s = {- (putStrLn ("Process on " ++ show s) >>) $ -} maybe
                (takeMVar c >>= incoming) 
                (\(op, s') -> tryTakeMVar c >>= maybe (perform f op >> process c f s') incoming) $ 
                nextOp s where
  incoming :: DevOp -> IO ()
  incoming = (>>= process c f) . either 
             (return . flip insertOp s) 
             (\op -> emptySchedule <$ 
                     (performAll f s >> case op of
                         Sync k -> fileFlush f >> k
                         FullBarrier -> fileFlush f
                     )
             ) {- . (\op -> trace ("incoming") op) -}
  
-- Seeks to the given position, expanding the file as necessary
hSeekX :: Handle -> ByteAddr -> IO ()
hSeekX h seek = do
    size <- fromIntegral <$> hFileSize h
    when (size <= seek) $ do
      hSeek h AbsoluteSeek (fromIntegral size)
      hWriteZeros h $ seek - size
    hSeek h AbsoluteSeek (fromIntegral seek)

hWriteZeros :: Handle -> ByteAddr -> IO ()
hWriteZeros h numZeros = when (numZeros > 0) $ do
  let batch = min numZeros (16*1024*1024)
  B.hPut h (B.replicate (fromIntegral batch) 0)
  hWriteZeros h (numZeros - batch)

newtype ChildException = ChildException SomeException deriving (Typeable, Show)
instance Exception ChildException

-- | Opens the specified file as a LocalStoreFile, runs the provided function and closes the file.
-- Do not make concurrent calls on the same file, place concurrency in the passed function.
withFileStoreFile :: FilePath -> (LocalStoreFile -> IO a) -> ErrorT String IO a
withFileStoreFile path user = lift $ withBinaryFile path ReadWriteMode $ \h -> hSetBuffering h NoBuffering >> withRawFile h user

-- | Opens the specified raw device as a LocalStoreFile, runs the provided function and closes the device.
-- Do not make concurrent calls on the same device, place concurrency in the passed function.
withRawDeviceStoreFile :: FilePath -> (LocalStoreFile -> IO a) -> ErrorT String IO a
withRawDeviceStoreFile path user =
  ErrorT $ bracket (openFd path ReadWrite Nothing $ defaultFileFlags {exclusive = True, append = True}) closeFd $ 
  \fd -> runErrorT $
         do fs <- lift $ getFdStatus fd
            bool (throwError "Not a raw device") (lift $ withRawFile (RawDevice fd fs 9) user) $ isCharacterDevice fs

-- | Like nesting multiple calls to 'withRawDeviceStoreFile'.
withRawDeviceStoreFiles :: [FilePath] -> ([LocalStoreFile] -> IO a) -> ErrorT String IO a
withRawDeviceStoreFiles ps user = foldr (\p u fs ->  (>>= ErrorT . pure) $ withRawDeviceStoreFile p $ \f -> runErrorT $ u $ fs ◊ [f]) (lift . user) ps []

toFileOffset :: Integral n => Len Word8 n -> FileOffset
toFileOffset = fromIntegral . getLen

toByteCount :: Integral n => Len Word8 n -> ByteCount
toByteCount = fromIntegral . getLen

fdSeekLen :: Fd -> ByteAddr -> IO ()
fdSeekLen fd seek = () <$ fdSeek fd AbsoluteSeek (toFileOffset seek)

-- TODO: consider adding support for a 'STPrimArray RealWorld Pinned Block', and a matching address type, that would enfoce the above requirements
-- However we would have to cast/view it as an array of Word8 later on.
-- | Array's size and start must be aligned on the block size, and the ByteAddr too.
fdReadArray :: Fd -> ByteAddr -> ArrayRange (STPrimArray RealWorld Pinned Word8) -> ErrorT String IO ()
fdReadArray fd start a = ErrorT $ fmap (boolEither "" () . (==) (toByteCount $ arrayLen a)) $ 
                         fdSeekLen fd start >> withArrayPtr (\ptr len -> fdReadBuf fd ptr $ toByteCount len) a

fdWriteArray :: Fd -> ByteAddr -> ArrayRange (STPrimArray RealWorld Pinned Word8) -> ErrorT String IO ()
fdWriteArray fd start a = ErrorT $ fmap (boolEither "" () . (==) (toByteCount $ arrayLen a)) $ 
                          fdSeekLen fd start >> withArrayPtr (\ptr len -> fdWriteBuf fd ptr $ toByteCount len) a

-- A bit of info on raw devices that i did not find easily elsewhere: http://www.win.tue.nl/~aeb/linux/lk/lk-11.html#ss11.4

data RawDevice = RawDevice Fd FileStatus Int
rawDeviceBlockBytes :: RawDevice -> Len Word8 Word
rawDeviceBlockBytes (RawDevice _ _ lg) = unsafeLen $ 1 `shiftL` lg
instance Show RawDevice where show (RawDevice _ fs _) = show $ specialDeviceID fs
-- TODO merge consecutive writes to improve performance (avoids many needless reads to preserve data that will be overwritten)
instance RawFile RawDevice where                              
  fileWriteRaw r@(RawDevice fd _ _) start bufs =
    let len = up $ sum $ arrayLen <$> bufs in
    withBlockArray r start len $ ((. fullArrayRange) .) $ \tStart t -> 
    do
      let bb = rawDeviceBlockBytes r
      let tLen = arrayLen t
      let tEnd = tStart + up tLen
      when (tStart < start) $ fdReadArray fd tStart $ headArrayRange bb t
      when (start + len < tEnd) $ fdReadArray fd (tEnd - up bb) $ skipArrayRange (tLen - bb)  t
      let dest = skipArrayRange (fromJust $ unapply super $ start - tStart) t
      _ <- lift $ stToIO $ foldlM (\d b -> skipArrayRange (arrayLen b) d <$ mapMArrayCopyImm return b d) dest bufs
      fdWriteArray fd tStart t
  fileReadRaw r@(RawDevice fd _ _) start buf =
    withBlockArray r start (up $ arrayLen buf) $ ((. fullArrayRange) .) $ \tStart t -> 
     do 
       -- liftIO $ putStrLn $ "Before fdReadArray " ++ show start
       fdReadArray fd tStart t
       let rangeToCopy = skipArrayRange (fromJust $ unapply super $ start - tStart) t
       lift $ stToIO (mapMArrayCopy return rangeToCopy buf)
  fileFlush _ = return ()
  
-- Takes start and length, and passes rounded start and an aligned buffer
withBlockArray :: MonadIO m => RawDevice -> ByteAddr -> ByteAddr -> (ByteAddr -> STPrimArray RealWorld Pinned Word8 -> m a) -> m a
withBlockArray r@(RawDevice _ _ lgBlockBytes) seek len f = 
  let blockBytes = rawDeviceBlockBytes r
      seek' = getLen seek
      len' = getLen len
      start = (seek' `shiftR` lgBlockBytes) `shiftL` lgBlockBytes
      end = ((seek' + len' + up (getLen blockBytes) - 1) `shiftR` lgBlockBytes) `shiftL` lgBlockBytes
  in liftIO (stToIO $ newAlignedPinnedWord8Array blockBytes $ unsafeLen $ fromJust $ unapply super $ end - start) >>= 
     f (unsafeLen start)
           -- . trace ("withBlockArray blockBytes=" ++ show blockBytes ++ " start=" ++ show (unsafeLen start) ++ " size=" ++ (show $ arrayLen r))
  
withRawFile :: (RawFile f, Show f) => f -> (LocalStoreFile -> IO a) -> IO a
withRawFile f user = do
  chan <- newEmptyMVar
  runWithDeamon 
    ("User of " ++ show f, user $ LocalStoreFile chan) 
    ("Server for " ++ show f, bracket_ (return ()) (return (){-putStrLn $ "Server for " ++ show f ++ " ended"-}) $ process chan f (emptySchedule :: CLook))
