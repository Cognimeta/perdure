{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, TupleSections #-}

module Cgm.Control.Concurrent.TThread (
  run2,
  runWithDeamon
  ) where

import Prelude hiding (catch)
import Data.IntMap
import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Cgm.Control.Combinators
import Cgm.Data.Bool

data Task n a = Task n ((Task n a -> IO ()) -> IO (a -> a))
data State n a = State {val :: a, nextId :: Int, threads :: IntMap (n, ThreadId, Bool), forcedCancel :: Bool}

data Abort = Abort deriving (Show, Typeable)
instance Exception Abort -- Not exported since it should not be thrown by anyone else 
-- Could malicious code still throw it, by catching it as SomeException, unpacking the exitential, and using throwTo on  
-- itself (myThreadId) or some other thread which has leaked its identity?

-- When a task completes, it applies a transition function to the candidate return value. Whenever the cancel predicate is
-- true for that value, or if a forced cancellation has been triggerred by an exception in a child thread or an abort request
-- in the parent, then all tasks that have not yet been cancelled are cancelled. Tasks starting will start cancelled if
-- the cancel predicate is true on the current value, or if forced cancellation has been triggerred.
-- Users wil probably want to ensure that once the cancel predicate becomes true, it never becomes false again.
-- This function should not be used with asynchronous exceptions in the parent thread (beyond the Abort exception
-- that may be thrown by an outer invocation of this function). Any unhandled exception in a child will be rethrown (wrapped)
-- in the parent, after all children have been cancelled and have completed. The user should only rely on this behavior
-- to handle unexepected exceptions. Other exceptions should be caught and transformed into a value in the children, so
-- that the cancellation predicate can determine if cancellation is appropriate, and that the result transformation function
-- can determine the appropriate result (the caller of run can transform back some values into exceptions).
run :: forall n a. [Task n a] -> a -> (a -> Bool) -> IO a
run tasks initial cancel = newChan >>= run' where
  run' c = foldM (flip startTransition) (State initial 0 Data.IntMap.empty False) tasks >>= handler where
    handler s = do 
      state'@(State a _ ts _) <- readChan c >>= ($ s)
      bool (handler state') (return a) $ Data.IntMap.null ts
    startTransition :: Task n a -> State n a -> IO (State n a)
    startTransition (Task n f) (State a i ts fc) = do
      t <- forkIO (f (writeChan c . startTransition) >>= writeChan c . endTransition)
      let cancelT = fc || cancel a
      when cancelT $ throwTo t Abort
      return $ State a (i+1) (insert i (n, t, cancelT) ts) fc where
        endTransition :: (a -> a) -> State n a -> IO (State n a)
        endTransition af (State a next ts fc) = do
          let ts' = delete i ts
          let a' = af a
          ts'' <- bool (return ts') (foldWithKey cancelNonCancelled (return ts') ts') $ not fc && cancel a'
          return (State a' next ts' fc) where
            cancelNonCancelled :: Int -> (n, ThreadId, Bool) -> Id (IO (IntMap (n, ThreadId, Bool)))
            cancelNonCancelled i (n, t, x) = if x then id else (>>= (<$ throwTo t Abort) . adjust (const (n, t, True)) i)

-- A task represented as pair of a description, and an IO of a triple containing: 
--  1) whether to attempt to cancel the other task, 
--  2) our result when we finish first, which the other task will convert into a final result
--  3) a function from that result of the other task to the final result, to be used only if we finish second
type Task2 c a b = (String, IO (Task2Result c a b))
type Task2Result c a b = ((Bool, a), b -> c)

data UnexpectedTaskException = UnexpectedTaskException Bool String SomeException deriving (Show, Typeable)
data ConcurrentExceptions = ConcurrentExceptions SomeException SomeException deriving (Show, Typeable)
instance Exception UnexpectedTaskException
instance Exception ConcurrentExceptions

data PeerTaskException = PeerTaskException deriving (Show, Typeable)
instance Exception PeerTaskException


-- Unexpected exceptions in a child will be wrapped in an UnexpectedTaskException, and an asynchronous
-- exception PeerTaskException will be thrown in the other task (if it has not already completed). In that case
-- the PeerTaskException is expected by the run2 method, so it does not have to be handled in the child.
-- If both children return an exception, then both are wrapped and the resulting pair is thrown as a ConcurrentExceptions.
-- Child tasks are required to catch Abort (if the peer requests it), and produce a Task2Result
-- TODO handle Abort in parent 

run2 :: Task2 c a b -> Task2 c b a -> IO c
run2 (n1, task1) (n2, task2) = do
  h1@(_, m1) <- forkIOT task1
  h2@(_, m2) <- forkIOT task2
  let w1 = UnexpectedTaskException False n1
  let w2 = UnexpectedTaskException True n2
  join $ atomically $ getEitherJust (firstComplete w2 h2 w1) (firstComplete w1 h1 w2) m1 m2 where
    firstComplete :: Wrapper -> ThreadHandles (Task2Result c b a) -> Wrapper -> Either SomeException (Task2Result c a b) -> IO c
    firstComplete wl (tl, ml) wf = either ex normal where
      ex ef = do
        throwTo tl PeerTaskException
        atomically (getJust ml) >>= either exl (const $ throw wrappedFirst) where
          wrappedFirst = wf ef
          exl el = maybe twoExceptions (const $ throw wrappedFirst) (fromException el :: Maybe PeerTaskException) where
            twoExceptions = throw $ ConcurrentExceptions (SomeException $ wl el) (SomeException wrappedFirst)
      normal ((cancel, intermediate), _) = do
        when cancel $ throwTo tl Abort
        final <- atomically (getJust ml) >>= either (throw . wl) (return . snd)
        return $ final intermediate
type Wrapper = SomeException -> UnexpectedTaskException
          
runWithDeamon :: (String, IO c) -> (String, IO ()) -> IO c
runWithDeamon (n1, f1) (n2, f2) = run2
                                 (n1, handle (\(e::Abort) -> error deamonEnded) f1 >>= \c -> return ((True, c), error deamonEnded)) 
                                 (n2, handle (\(e::Abort) -> return ()) f2 >> return ((True, ()), id)) where
  deamonEnded = "Deamon ended spontaneously"

type ThreadHandles a = (ThreadId, STM (Maybe (Either SomeException a)))

forkIOT :: IO a -> IO (ThreadHandles a)
forkIOT f = do
  v <- atomically $ newTVar Nothing
  t <- forkIO $ handle (set v . Left) (f >>= set v . Right)
  return (t, readTVar v) where
    set v = atomically . writeTVar v . Just

getJust :: STM (Maybe a) -> STM a
getJust = (>>= maybe retry return)

getEitherJust :: (a -> z) -> (b -> z) -> STM (Maybe a) -> STM (Maybe b) -> STM z
getEitherJust z1 z2 m1 m2 = m1 >>= maybe (m2 >>= maybe retry (return . z2)) (return . z1)
