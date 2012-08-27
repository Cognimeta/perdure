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

module Database.Perdure.TestState (
  testStates,
  testStatesDag,
  testStatesDestroysRaw1,
  testInitState,
  testRootAddresses,
  SRef(..)
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

-- | A reference type which automatically puts its referent is a separately loadable allocation when its size is >= 2^12 bytes (4K)
type SRef = CRef (SizeRef D12)

-- | A list with an SRef that automatically cuts it up into separately loadable segments of more than 4K.
data RList a = EmptyRList | ConsRList a (SRef (RList a)) deriving (Show, Typeable)
instance (Persistent a, Typeable a) => Persistent (RList a) where persister = structureMap persister

testStates  :: a -> IO ()
testStates _ =
  quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ (>>= either fail return) $ runErrorT $ (True <$) $
  withReplicatedFiles "testStates" testStatesF

testStatesF :: ReplicatedFile -> IO ()
testStatesF f = do
    i <- testInitState f
    t0 <- writeState EmptyRList i
    foldM (\s c -> putStrLn (show c) >> Std.execStateT (toStandardState $ updateState $ stepN c) s) t0 [0 .. 19]
    return () where
      stepN :: Word32 -> StateT (RList Word32) IO ()
      stepN c = get >>= \l -> put $ foldr (\_ -> ConsRList c . ref) l $ counting 5000

-- | The root locations we will use here are two consecutive roots
testRootAddresses :: [RootAddress]
testRootAddresses = [RootAddress 0, RootAddress $ apply super rootAllocSize]

-- | Create an initial state where the is 800MB free space (beyond the two consecutive roots specified in testRootAddresses)
testInitState :: WriteStoreFile -> IO (RootState No MapMultiset SpaceTree a)
testInitState f = flip fmap (newMVar $ emptyCache 1000) $ \c -> 
  initState (StateLocation f c testRootAddresses) $
  addSpan (sortedPair (2 * apply super (getLen rootAllocSize)) $ 100 * 1000000) emptySpace

withReplicatedFiles :: String -> (ReplicatedFile -> IO a) -> ErrorT String IO a
withReplicatedFiles n z = ErrorT $ fmap join $
                         runErrorT $ withFileStoreFile (n ++ "0.dag") $ \f0 ->
                         runErrorT $ withFileStoreFile (n ++ "1.dag") $ \f1 ->
                         z $ ReplicatedFile [f0, f1]

-- | Here we perform the test on a (single) raw device
testStatesDestroysRaw1  :: a -> IO ()
testStatesDestroysRaw1 _ =
  quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ (>>= either fail return) $ runErrorT $ (True <$) $
  withRawDeviceStoreFile "/dev/raw/raw1" $ (. (ReplicatedFile . pure)) $ testStatesF

----------

-- | A Rose tree of empty nodes, each separately loadable (here we do not use SizeRef), to test complex acyclic graphs.
data Dag = Dag (CDRef [Dag]) deriving Typeable

dagChildren :: Dag -> [Dag]
dagChildren (Dag r) = deref r

instance Persistent Dag where persister = structureMap persister

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

testStatesDag  :: a -> IO ()
testStatesDag _ =
  quickCheckWith (Args Nothing 1 1 1 True) $ morallyDubiousIOProperty $ (>>= either fail return) $ runErrorT $ (True <$) $
  withReplicatedFiles "testStatesDag" $ \f -> 
  do
    i <- testInitState f
    t0 <- writeState (Dag $ ref []) i
    foldM (\s c ->
            putStrLn (show c) >>
            Std.execStateT (toStandardState $ updateState $ StateT $ fmap (((),) . Just) . evalRandIO . dagBuild) s)
      t0 $ [0 .. 1999]

deriveStructured ''RList
deriveStructured ''Dag