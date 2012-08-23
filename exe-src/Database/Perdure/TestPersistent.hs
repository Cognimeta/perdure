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
  testSeqPersistent,
  testStates,
  testState2,
  testState2Dag,
  testStatesDestroysRaw1
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
import Database.Perdure.Count.Count(Address)
import Cgm.System.Endian
import Debug.Trace
import Profile
import qualified Control.Monad.State.Strict as Std
import Cgm.Control.Monad.State
import Cgm.Data.Either
import Control.Monad.Error
import qualified Cgm.Data.Persist.Map as PMap
import Database.Perdure.Ref
import Cgm.Data.Super
import Database.Perdure.RNF
import Database.Perdure.CSerializer
import Database.Perdure.CDeserializer
import Database.Perdure.RNF
import Cgm.Data.Nat
import Cgm.Data.WordN
import Cgm.Data.Persist.MapMultiset
import Database.Perdure.Space.SpaceTree
import Control.Monad.Random
import Control.Concurrent.MVar
import Cgm.Data.Typeable

type SRef = CRef (SizeRef D12)
data TestData a = EmptyTestData a | NodeTestData [a] [SRef (TestData a)] deriving (Show, Eq, Typeable)
generateTestData :: (Persistent a, Num a, Ix a) => Int -> TestData a
generateTestData n = if (n==0) then EmptyTestData 0
                               else NodeTestData (range (0, fromIntegral n)) $ ref <$> generateTestData <$> range (0, n - 1)
instance (Persistent a, Typeable a) => Persistent (TestData a) where persister = structureMap $ persister |. persister &. persister

instance (Persistent a, Typeable a, Arbitrary a) => Arbitrary (TestData a) where
  arbitrary = sized g where
    g n = bool (EmptyTestData <$> arbitrary) (NodeTestData <$> listOf arbitrary <*> promote ((ref <$>) . g <$> range (0, n - 1))) $ n > 0

-- | Create an initial state where the is 800MB free space (beyond the two consecutive roots specified in testRootAddresses)
testInitState :: WriteStoreFile -> MVar Cache -> RootState No [] SpaceTree a
testInitState f c =
  initState (StateLocation f c testRootAddresses) $
  addSpan (sortedPair (2 * apply super (getLen rootAllocSize)) $ 100 * 1000000) emptySpace

-- | The root locations we will use here are two consecutive roots
testRootAddresses :: [RootAddress]
testRootAddresses = [RootAddress 0, RootAddress $ apply super rootAllocSize]

writeReadTestFile :: forall a. (Eq a, Persistent a, Typeable a) => a -> String -> IO Bool
writeReadTestFile a name = fmap fromRight $ runErrorT $ withFileStoreFile name $ (. (ReplicatedFile . pure)) $ \f -> do
  putCpuTime "Data creation" $ evaluate $ prnf persister a
  init <- testInitState f <$> newMVar (emptyCache 1000)
  final <- writeState a init >>= writeState a
  readCache <- newMVar (emptyCache 1000)
  readState :: RootState Identity [] SpaceTree a <- maybe (error "No valid roots") id <$>
                                                    readState (StateLocation f readCache testRootAddresses)
  putCpuTime "Read time" $ evaluate $ a == (runIdentity $ stateValue readState)


testPersistent  :: a -> IO ()
testPersistent args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $
                      writeReadTestFile (generateTestData 15 :: TestData Word32) "testPersistent.dag"

testPersistentMap  :: a -> IO ()
testPersistentMap args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $
                         writeReadTestFile (foldl' (\z n -> PMap.insert n n z) PMap.empty [(1 :: Integer) .. 10000])  "testPersistentMap.dag"

data RList a = EmptyRList | ConsRList a (SRef (RList a)) deriving (Show, Typeable)
instance (Persistent a, Typeable a) => Persistent (RList a) where persister = structureMap persister

data Dag = Dag (CDRef [Dag]) deriving Typeable
dagChildren :: Dag -> [Dag]
dagChildren (Dag r) = deref r
instance Persistent Dag where persister = structureMap persister

testStates  :: a -> IO ()
testStates args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ fmap fromRight $ runErrorT $ join $
                  fmap (ErrorT . pure) $ withFileStoreFile "testStates0.dag" $ \f0 ->
                  runErrorT $ withFileStoreFile "testStates1.dag" $ \f1 -> testStates2 $ ReplicatedFile [f0, f1]

testStates2 :: ReplicatedFile -> IO Bool
testStates2 f = do
  cache <- newMVar (emptyCache 1000)
  t0 <- writeState EmptyRList $ testInitState f cache
  True <$ (foldM (\s c -> putStrLn (show c) >> Std.execStateT (toStandardState $ updateState $ mapStateT lift $ stepN c) s) t0 $ counting 20)

testStatesDestroysRaw1  :: a -> IO ()
testStatesDestroysRaw1 args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ fmap fromRight $ runErrorT $ 
                  withRawDeviceStoreFile "/dev/raw/raw1" $ (. (ReplicatedFile . pure)) $ testStates2

stepN :: Word32 -> StateT (RList Word32) IO ()
stepN c = get >>= \l -> put $ foldr (\_ -> ConsRList c . ref) l $ counting 5000

----------

testState2  :: a -> IO ()
testState2 args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ fmap fromRight $ runErrorT $ join $
                  fmap (ErrorT . pure) $ withFileStoreFile "testState2_0.dag" $ \f0 ->
                  runErrorT $ withFileStoreFile "testState2_1.dag" $ \f1 -> testState2_2 $ ReplicatedFile [f0, f1]

testInitState2 :: WriteStoreFile -> MVar Cache -> RootState No MapMultiset SpaceTree a
testInitState2 f c = initState (StateLocation f c testRootAddresses) $
                     addSpan (sortedPair (2 * apply super (getLen rootAllocSize)) 100000000) emptySpace

testState2_2 :: ReplicatedFile -> IO Bool
testState2_2 f = do
  cache <- newMVar (emptyCache 1000)
  t0 <- writeState EmptyRList $ testInitState2 f cache
  True <$ (foldM (\s c -> putStrLn (show c) >> 
                          Std.execStateT (toStandardState $ bool id (>>= (<$ collectStateM)) (c `mod` 5 == 0) $ 
                                          updateState $ mapStateT lift $ stepN c) 
                          s) t0 $ counting 20)

-- Selects an element at random in the dag, and can also return Nothing
dagElem :: RandomGen g => Dag -> Rand g (Maybe Dag)
dagElem d = getRandomR (0 :: Int, 9) >>= \r -> if r == 0 then return $ Just d else 
                                         let c = dagChildren d 
                                         in if null c then return Nothing else getRandomR (0, length c - 1) >>= dagElem . (c !!)
                        
-- Like dagElem but when Nothing would be returned d is returned. 
dagElem' :: RandomGen g => Dag -> Rand g Dag
dagElem' d = fmap (fromMaybe d) $ dagElem d

dagBuild :: RandomGen g => Dag -> Rand g Dag
dagBuild d = fmap (Dag . ref) $ Cgm.Prelude.sequence $ replicate 10 (dagElem' d)

testState2Dag  :: a -> IO ()
testState2Dag args = quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ fmap fromRight $ runErrorT $ join $
                fmap (ErrorT . pure) $ withFileStoreFile "testState2Dag_0.dag" $ \f0 ->
                runErrorT $ withFileStoreFile "testState2Dag_1.dag" $ \f1 -> 
                let f = ReplicatedFile [f0, f1] in
                do
                  cache <- newMVar (emptyCache 1000)
                  t0 <- writeState (Dag $ ref []) $ testInitState2 f cache
                  True <$ (foldM (\s c -> putStrLn (show c) >> 
                                              Std.execStateT (toStandardState $ updateState $ mapStateT lift $ StateT $ fmap (((),) . Just) . evalRandIO . dagBuild) 
                                              s) t0 $ counting 2000)

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



deriveStructured ''TestData
deriveStructured ''RList
deriveStructured ''Dag