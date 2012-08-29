{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, TupleSections, DeriveDataTypeable #-}

-- To view the modules dependencies in Database.Perdure:
-- graphmod Database.Perdure.TestPersistent -R Cgm.Data -R Cgm.System -R Cgm.Control -r Cgm.Prelude -r Profile | dot -Tpng -oCgm_Persist.png

module Database.Perdure.TestPersistent (
  testPersistentMap,
  testPersistent,
  testSeqPersistent
  ) where

import Prelude()
import Cgm.Prelude
import Control.Exception
import Control.DeepSeq
import Data.Ix
import Data.Word
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Property
import Database.Perdure.State
import Database.Perdure.SizeRef
import Database.Perdure.RNF
import Database.Perdure.ReplicatedFile
import Cgm.Control.Combinators
import Database.Perdure.Count(Address)
import Cgm.System.Endian
import Debug.Trace
import Cgm.Control.Profile
import qualified Control.Monad.State.Strict as Std
import Cgm.Control.Monad.State
import Cgm.Data.Either
import Control.Monad.Error
import qualified Database.Perdure.Data.Map as PMap
import Database.Perdure.Ref
import Cgm.Data.Super
import Database.Perdure.RNF
import Database.Perdure.CSerializer
import Database.Perdure.CDeserializer
import Database.Perdure.RNF
import Cgm.Data.Nat
import Cgm.Data.WordN
import Database.Perdure.Data.MapMultiset
import Database.Perdure.SpaceTree
import Control.Monad.Random
import Control.Concurrent.MVar
import Cgm.Data.Typeable
import Database.Perdure.TestState
import Database.Perdure

-- | We test with some type of tree with variable arity and nodes of various sizes.
data TestTree a = Leaf a | Node [a] [SRef (TestTree a)] deriving (Show, Eq, Typeable)

-- | Some regular values for TestTree of growing complexity.
generateTestTree :: (Persistent a, Num a, Ix a) => Int -> TestTree a
generateTestTree n =
  if n==0
  then Leaf 0
  else Node (range (0, fromIntegral n)) $ ref <$> generateTestTree <$> range (0, n - 1)

instance (Persistent a, Typeable a, Arbitrary a) => Arbitrary (TestTree a) where
  arbitrary = sized g where
    g n = bool (Leaf <$> arbitrary) (Node <$> listOf arbitrary <*> promote ((ref <$>) . g <$> range (0, n - 1))) $ n > 0

-- | We establish a default persister for TestTree, which will simply persist it according to the internal structure.
instance (Persistent a, Typeable a) => Persistent (TestTree a) where persister = structureMap persister

testInitState :: (Typeable a, Persistent a) => ReplicatedFile -> a -> IO (PState a)
testInitState f a = fmap defaultRootLocation (newCachedFile 1000 f) >>= \l -> 
  initState l
  (addSpan (sortedPair (2 * apply super (getLen rootAllocSize)) $ 100 * 1000000) emptySpace)
  a

writeReadTestFile :: (Eq a, Persistent a, Typeable a) => a -> String -> IO Bool
writeReadTestFile a name = fmap fromRight $ runErrorT $ withFileStoreFile name $ (. (ReplicatedFile . pure)) $ \f -> do
  putCpuTime "Data creation" $ evaluate $ prnf persister a
  putCpuTime "Write time" $
    newCachedFile 1000 f >>=
    createPVar a (mega 100) . defaultRootLocation
  putCpuTime "Read time" $
    newCachedFile 1000 f >>=
    fmap (fromMaybe $ error "Read error") . openPVar . defaultRootLocation >>= \v ->
    updatePVar v $ get >>= liftIO . evaluate . (a ==)


testPersistent  :: a -> IO ()
testPersistent args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $
                      writeReadTestFile (generateTestTree 15 :: TestTree Word32) "testPersistent.dag"

testPersistentMap  :: a -> IO ()
testPersistentMap args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $
                         writeReadTestFile (foldl' (\z n -> PMap.insert n n z) PMap.empty [(1 :: Integer) .. 10000])  "testPersistentMap.dag"

propPersister :: forall a p. (Show a, Eq a) => Persister a -> a -> Property
propPersister p a = morallyDubiousIOProperty $ return $ (== a) $ 
                    deserializeFromFullArray (unsafeSeqDeserializer p) $ fullArrayRange $ (id :: Id (PrimArray Pinned Word64)) $ serializeToArray p a

propPersistent :: (Show a, Eq a, Persistent a) => a -> Property
propPersistent = propPersister persister

myCheck :: Testable p => Int -> p -> IO ()
myCheck n = quickCheckWith (Args Nothing n n n True)

testSeqPersistent :: a -> IO ()
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
  myCheck 20 $ propPersister ((persister >. (persister :: Persister (RWord8 D7))) &. persister) . (id :: Id (Word8, Word32))
  myCheck 20 $ propPersister ((persister >. (persister :: Persister (RWord64 D63))) &. persister) . (id :: Id (Word64, Word32))

deriveStructured ''TestTree
