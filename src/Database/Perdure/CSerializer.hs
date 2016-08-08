{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, TupleSections, ScopedTypeVariables, MagicHash, UnboxedTuples, BangPatterns, GADTs #-}

module Database.Perdure.CSerializer (
  SerializerContext,
  cSer,
  serializeToArray,
  Address,
  module Database.Perdure.CDeserializer,
  module Cgm.Control.Concurrent.NotificationCount
  ) where

import Prelude ()
import Cgm.Prelude

import Control.Concurrent
import GHC.Base hiding ((.), id)
import GHC.IO hiding (liftIO)
import Cgm.Data.Functor.Sum
import Cgm.Data.Word
import Cgm.Control.Concurrent.NotificationCount
import Database.Perdure.CDeserializer
import Database.Perdure.Count
import Cgm.Data.Multiset as MS
import Database.Perdure.AllocCopy
import Cgm.Data.Maybe
import Database.Perdure.Allocator
import Database.Perdure.ArrayRef
import Database.Perdure.WordArrayRef()
import Database.Perdure.WordNArrayRef()
import qualified Database.Perdure.Cache as Cache
import Cgm.Data.MapMultiset
import Data.Bool
import Data.Dynamic
import Control.Monad.Random


-- Important : We must not read (deref) a ref that has just been written in the current writeState.
-- Since reodering of operations is allowed except across the storeFileFullBarrier (performed at the end of
-- write State), this ensures that we do not read data which has not yet been fully written.

-- TODO make sure just written ref's can be inserted in the cache, since that data is likely to be accessed again

type CountDest c = Maybe (MVar (c Address))
addCount :: Multiset c => Address -> CountDest c -> IO ()
addCount a = maybe (return ()) $ flip modifyMVar_ $ return . MS.insert a
type SerializerContext l c = (MVar Cache, l, CountDest c) -- TODO create type with strict fields

-- TODO: consider testing whether returning a CSer (Maybe a) might be more performant, where Nothing represent the input a

type Dest = ABitSeq RealWorld

cSer :: (Multiset c, Allocator l) => Persister a -> SerializerContext l c -> (a -> Dest -> IO z) -> a -> Dest -> IO z
cSer !p !sc !k !a !d = case p of
  PartialWordPersister n
    | n == wordBits -> stToIO (addWord a d) >>= k a
    | n == 0 -> k a d
    | otherwise -> stToIO (addBits n a d) >>= k a 
  PairPersister pb pc -> case a of (b, c) -> cSer pb sc (\b' -> cSer pc sc (\c' -> k (b', c')) c) b d
  EitherPersister pb pc -> either (\b -> stToIO (addBit 0 d) >>= cSer pb sc (k . Left) b) (\c -> stToIO (addBit 1 d) >>= cSer pc sc (k . Right) c) a
  ViewPersister i pb -> cSer pb sc (k . fromJust . unapply i) (apply i a) d
  SummationPersister pi' _ s -> s (\i pb ba b -> cSer pi' sc (const $ cSer pb sc (k . ba) b) i d) a -- i' is ignored, i type should not contain CRefs
  DRefPersister' -> case a of (DRef _ _ w) -> 
                                case sc of (_, _, c) -> addCount (arrayRefAddr w) c >> 
                                                        cSer persister sc (const $ k a) w d -- TODO verify if call to persister is wastful
  CRefPersister' rp prb -> onCRef (\rb -> cSer prb sc (const $ k a) rb d) (\b -> cSerRef rp sc (k . Refed) b d) a  

-- Creates the ref and serializes it
cSerRef :: (Multiset c, Allocator l, Persistent a, Typeable a) => RefPersister r -> SerializerContext l c -> (r a -> Dest -> IO z) -> a -> Dest -> IO z
cSerRef !rp !sc !k !a !d = case rp of
  Ref0Persister -> cSer persister sc (k . Ref0) a d
  RefView rbra pb -> cSerRef pb sc (k . rbra) a d
  SizeRefPersister maxSize -> 
    let p = persister  -- TODO verify if call to persister is wastful
        start = d
        asDRef (Ref0 a') dStart dEnd = do
          --readIORef writeCount >>= putStr . (++ ",") . show
          --readIORef writeCount >>= writeIORef writeCount . (+ 1)
          --empty <- stToIO $ mkABitSeq
          --putStrLn $ ("CSerializer: Popped size = " ++) $ showLen $ addedBits dEnd dStart
          --putStrLn $ ("CSerializer: Stack size = " ++) $ showLen $ addedBits dStart empty
          dRef <- mkDRef sc p (Just a') dStart dEnd
          afterTrue <- stToIO $ addBit 1 start
          cSer DRefPersister' (noCount sc) (k . Sum . Right) dRef afterTrue -- Doesn't pass CountDest. We don't count first references.
    in 
     stToIO (addBit 0 start) >>= \afterFalse -> 
     cSerRef Ref0Persister sc 
     (\r0 end0 -> bool (k (Sum $ Left r0) end0) (asDRef r0 afterFalse end0) (addedBits end0 afterFalse > maxSize)) a afterFalse
  CRefPersister _ -> error "CRef of CRef not yet implemented" -- TODO write
  DRefPersister -> 
    let p = persister in -- TODO verify if call to persister is wastful
    stToIO mkABitSeq >>= \start -> cSer p sc (\a' end -> mkDRef sc p (Just a') start end >>= \dRef -> cSer DRefPersister' (noCount sc) k dRef d) a start
        -- Doesn't pass CountDest. We don't count first references.q
        -- TODO optimization : if the WordSeq fits in a single chunk, we should be able to writeDRef directly from it, since it is already aligned
  IRefPersister pb -> cSerRef pb sc (k . IRef) a d
  
noCount :: SerializerContext l c -> SerializerContext l c
noCount (cache, l, _) = (cache, l, Nothing)

mkDRef :: (Allocator l, BitSrc s, SrcDestState s ~ RealWorld, Typeable a) => 
          SerializerContext l c -> Persister a -> Maybe a -> s -> s -> IO (DRef a)
mkDRef (cache, l, _) p ma = writeDRef p ma (DeserializerContext (allocatorStoreFile l) cache) l

writeDRef :: (Allocator l, BitSrc s, SrcDestState s ~ RealWorld, Typeable a) => 
             Persister a -> Maybe a -> DeserializerContext -> l -> s -> s -> IO (DRef a)
writeDRef p ma dc@(DeserializerContext _ c) l start end = 
  DRef p dc <$> maybe id (\a -> (>>= \w -> w <$ addToCache a w)) ma (allocCopyBits start end >>= writeArrayRef l) where
  addToCache a w = let addr = arrayRefAddr w in modifyMVar_ c $ 
                                                evalRandIO . {-(trace ("writing/adding to cache at" ++ show addr) $ -}
                                                Cache.insert addr (Cache.Entry (toDyn a) $ arrayRefSize w)
  
-- | The passed Persister must hace no references
{-# NOINLINE serializeToArray #-}
serializeToArray :: AllocCopy w => Persister a -> a -> PrimArray Pinned w
serializeToArray p a = unsafePerformIO $ do
  start <- stToIO mkABitSeq 
  noCache <- newMVar $ Cache.empty 0
  cSer p (noCache, NoAllocator, Nothing :: CountDest MapMultiset) (const $ allocCopyBits start) a start
                             
data NoAllocator = NoAllocator
instance Allocator NoAllocator where
  alloc = error "NoAllocator"
  allocatorStoreFile = error "NoAllocator"
    

