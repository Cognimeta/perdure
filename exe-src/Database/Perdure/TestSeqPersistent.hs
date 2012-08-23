{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Database.Perdure.TestSeqPersistent (
  ) where

import Prelude ()
import Cgm.Prelude

import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Property
import Database.Perdure.CSerializer
import Database.Perdure.CDeserializer
import Cgm.Control.Combinators
import Database.Perdure.WriteBits
import Database.Perdure.AllocCopy
import Database.Perdure.RNF
import Database.Perdure.WValidator
import Cgm.Data.Len
import Cgm.Data.Word
import Cgm.Data.Super
import Cgm.Data.Digest
import Cgm.Data.WordN
import Foreign.Marshal.Alloc
import Cgm.System.Mem.Alloc
import Profile

{-
propPersister :: forall a p. (Show a, Eq a) => (forall p. SeqPersister p => p a) -> a -> Property
propPersister p a = morallyDubiousIOProperty $ do
                    --putStrLn $ "\n" ++ show a
                    start <- stToIO mkABitSeq
                    end <- stToIO $ bSer (getSerializer p a) start
                    buf :: PrimArray Pinned Word <- allocCopyBits start end
                    -- putStrLn $ show $ unsafeGetAllocated aBuf
                    return $ (== a) $ deserValue $ deserializeFromArray (p :: Deserializer Pinned a) $ fullArrayRange buf

--propPersister p a = morallyDubiousIOProperty $
 --                   allocPSerialize EmptyBitSeq (apply pSer p a EmptyBitSeq) >>= flip freeAllocated (((== a) <$>) . deserializeBuffer p)

propPersistent :: (Show a, Eq a, SeqPersistent a) => a -> Property
propPersistent = propPersister seqPersister

myCheck n = quickCheckWith (Args Nothing n n n True)



testSeqPersistent args = do
  myCheck 5 $ propPersistent . (id :: Id Bool)
  myCheck 50 $ propPersistent . (id :: Id Word8)
  myCheck 50 $ propPersistent . (id :: Id (Word8, Word8))
  myCheck 50 $ propPersistent . (id :: Id Word16)
  myCheck 50 $ propPersistent . (id :: Id Word32)
  myCheck 50 $ propPersistent . (id :: Id (Word32, Word32))
  myCheck 50 $ propPersistent . (id :: Id Word64)
  myCheck 50 $ propPersistent . (id :: Id (Word8, Word64, Word8))
  myCheck 50 $ propPersistent . (id :: Id (RWord64 D63, Word64))
  myCheck 50 $ propPersistent . (id :: Id (RWord64 D32, Word64))
  myCheck 50 $ propPersistent . (id :: Id (Word8, Word16, Word32, Word64))
  myCheck 10 $ propPersistent . (id :: Id [Bool])
  myCheck 9 $ propPersistent . (id :: Id (Maybe Bool))
  myCheck 16 $ propPersistent . (id :: Id (Either Bool Bool))
  myCheck 50 $ propPersistent . (id :: Id ((Either (Either Word8 Word16) (Either Word32 Word64)), Word8))
  myCheck 20 $ propPersister ((seqPersister >. (seqPersister :: SeqPersister p => p (RWord8 D7))) &. seqPersister) . (id :: Id (Word8, Word32))
  myCheck 20 $ propPersister ((seqPersister >. (seqPersister :: SeqPersister p => p (RWord64 D63))) &. seqPersister) . (id :: Id (Word64, Word32))
    

testWriteSeqPersistent args = do
  let a = counting 10000000 :: [Word32]
  putCpuTime "Just the write bits" $ stToIO $ do
    start <- mkABitSeq
    foldlM (\b e -> addBit 1 b >>= addWord e) start (counting 10000000 :: [Word])
  putCpuTime "RNF" $ evaluate $ apply rnfF seqPersister a
  putCpuTime "Redundant RNF" $ evaluate $ apply rnfF seqPersister a
  start <- stToIO mkABitSeq
  --start' <- stToIO $ addBit 0 start
  end <- putCpuTime "Writing to ABitSeq" $ stToIO $ apply serializer seqPersister a start
  putStrLn $ show $ addedBits end start
  buf <- putCpuTime "Copying to Array" $ allocCopyBits start end
  putCpuTime "Hashing" $ either 
    (\c -> putStrLn $ show $ (id :: Id MD5Digest) $ digest $ apply (wordConv1 c) buf) 
    (\c -> putStrLn $ show $ (id :: Id (Skein512Digest Word128)) $ digest $ apply (wordConv1 c) buf) wordConv
-} 

--testPerformance args = do (v, t) <- cpuTimeNF $ foldr (&&) True $ fmap prop_Int64Inverses $ range (1, 1000000)
--                          putCpuTime "testPerformance" t
--                          return ()

