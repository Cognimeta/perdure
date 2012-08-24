{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Cgm.Control.Monad.State(
  StateT(..),
  State,
  mState,
  runState,
  focus,
  viewState,
  partialState,
  partialStateE,
  eitherState,
  maybeState,
  toStandardState,
  mapStateT,
  pairStateT,
  module Control.Monad.State.Class,
  module Control.Monad.Trans.Class,
  module Control.Monad.IO.Class
  ) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Functor.Identity
import Data.Maybe
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import qualified Control.Monad.State.Strict as Std

import Data.Lens hiding (focus)
import Control.Comonad.Trans.Store

-- | runStateT and runState do not have the usual types: for now we do not make it too easy to discard the precious 'Nothing'
newtype StateT s m a = StateT {runStateT :: s -> m (a, Maybe s)}

type State s = StateT s Identity
mState :: (s -> (a, Maybe s)) -> State s a 
mState = StateT . fmap Identity
runState :: State s a -> s -> (a, Maybe s)
runState = fmap runIdentity . runStateT

instance Functor m => Functor (StateT s m) where
  fmap f = StateT . fmap (fmap $ first f) . runStateT
instance (Functor m, Monad m) => Applicative (StateT s m) where  
  pure = return
  (<*>) = ap
instance Monad m => Monad (StateT s m) where
  return = lift . return
  (StateT c) >>= fd = StateT $ \s -> 
    c s >>= \(a, ms) ->   -- We are strict in the state. We do as in Control.Monad.Trans.State.Strict (not .Lazy)
    let d = runStateT (fd a) 
    in maybe (d s) (\s' -> liftM (second $ Just . fromMaybe s') $ d s') ms
  
instance MonadTrans (StateT s) where
  lift = StateT . const . liftM (, Nothing)
  
instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
  
instance Monad m => MonadState s (StateT s m) where
  get = StateT $ return . (, Nothing)
  put = StateT . const . return . ((), ) . Just

focus :: Monad m => Lens t s -> StateT s m a -> StateT t m a
focus l (StateT smas) = StateT $ (\(st, s) -> liftM (second $ fmap st) $ smas s) . runStore . runLens l

-- | functions st and ts should form a bijection since a StateT t with no changes will become a StateT s with no changes, no matter what st and ts are
viewState :: Monad m => (s -> t, t -> s) -> StateT t m a -> StateT s m a
viewState (st, ts) (StateT tf) = StateT $ liftM (second $ fmap ts) . tf . st

-- Work on a part of the state, that may not exist for some inputs
partialState :: Monad m => m a -> (s -> Maybe t, t -> s) -> StateT t m a -> StateT s m a
--partialState d (smt, ts) (StateT tf) = get >>= \s -> maybe (lift d) ((>>= \(a, mt) -> maybe (return ()) (put . ts) mt >> return a) . lift . tf) $ smt s
partialState d (smt, ts) t = viewState (\s -> maybe (Left s) Right $ smt s, either id ts) $ eitherState (lift d) t -- note that we use a view 's -> Either s t'

-- Like partialState, but remembers which path was taken
partialStateE :: Monad m => m a -> (s -> Maybe t, t -> s) -> StateT t m b -> StateT s m (Either a b)
partialStateE d fs t = partialState (liftM Left d) fs (liftM Right t)

eitherState :: Monad m => StateT s m a -> StateT t m a -> StateT (Either s t) m a
eitherState (StateT sf) (StateT tf) = StateT $ either (liftM (second $ fmap Left) . sf) (liftM (second $ fmap Right) . tf)
  
maybeState :: Monad m => m a -> StateT s m a -> StateT (Maybe s) m a
maybeState n s = viewState (maybe (Left ()) Right, either (const Nothing) Just) $ eitherState (lift n) s
  
toStandardState :: Monad m => StateT s m a -> Std.StateT s m a
toStandardState (StateT f) = Std.StateT $ \s -> liftM (second $ fromMaybe s) $ f s

mapStateT :: (m (a, Maybe s) -> n (b, Maybe s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

pairStateT :: Functor m => StateT s (StateT t m) a -> StateT (s, t) m a
pairStateT (StateT sf) = StateT $ \(s, t) -> fmap (\((a, ms), mt) -> (a, combineMaybes s t ms mt)) $ runStateT (sf s) t where
  combineMaybes s t ms = maybe (fmap (, t) ms) (Just . (fromMaybe s ms,))

