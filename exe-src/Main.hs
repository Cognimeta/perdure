{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, TemplateHaskell #-}

module Main (
  main

) where

import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary
import System.Environment
import Language.Haskell.TH
import Database.Perdure.TestPersistent
import Database.Perdure.TestSeqPersistent
import Database.Perdure.TestStoreFile
import Cgm.TH.Label
import Cgm.Data.TestMap

import Profile

main :: IO ()
main = getArgs >>= argsMain

argsMain :: [String] -> IO ()
argsMain (x:xs) = maybe invalidArgs ($ xs) $ lookup x mains
argsMain [] = invalidArgs

invalidArgs :: IO ()
invalidArgs = putStrLn $ "Invalid arguments, first argument should be one of " ++ (show $ map fst mains)

mains :: [(String, [String] -> IO ())]
mains = [
  $(label 'testFile),
    $(label 'testStoreFile),
  $(label 'testPersistent),
    $(label 'testPersistentMap),
  $(label 'testSeqPersistent),
    $(label 'testStates),
  $(label 'testState2),
    $(label 'testState2Dag),
  $(label 'testStatesDestroysRaw1),
    $(label 'testMap)
  ]
