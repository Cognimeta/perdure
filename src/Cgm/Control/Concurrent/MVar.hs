{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TupleSections #-}

module Cgm.Control.Concurrent.MVar (
  stateModifyMVar,
  stateTModifyMVar,
  withNewMVar,
  module Control.Concurrent.MVar
  ) where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad.State.Strict
import Data.Tuple
import Control.Exception

stateModifyMVar :: MVar a -> State a b -> IO b
stateModifyMVar v state = modifyMVar v (return . swap . runState state)

stateTModifyMVar :: MVar a -> StateT a IO b -> IO b
stateTModifyMVar v st = modifyMVar v ((>>= forceState) . fmap swap . runStateT st) where 
  forceState (a, b) = fmap (, b) $ evaluate a
  -- forceState fixes a selector leak. We do not want the mvar to hold a thunk that keeps b alive
  -- I no longer understand what I did here. modifyMVar seems already strict in the pair it receives
  -- But forcing the state is still not a bad idea...

-- z must not refer to the MVar t
withNewMVar :: (MVar t -> IO z) -> StateT t IO z
withNewMVar z = StateT $ (>>= \var -> liftA2 (,) (z var) (readMVar var)) . newMVar


