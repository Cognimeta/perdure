{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Control.Concurrent.Await (
  Async,
  await0,
  await1,
  mapAsync
  ) where

import Control.Concurrent
import Control.Monad

-- Takes a operation that takes a nullary callback, performs the operation and awaits the callback
await0 :: (IO () -> IO a) -> IO a
await0 kop = do v <- newEmptyMVar
                a <- kop (putMVar v ())
                takeMVar v
                return a

type Async a s = (a -> IO ()) -> IO s

-- Takes a operation that takes a unary callback, performs the operation, awaits the callback, and returns the value
await1 :: Async a () -> IO a
await1 kop = do v <- newEmptyMVar
                kop (putMVar v)
                takeMVar v

mapAsync :: (a -> IO b) -> Async a () -> Async b ()
mapAsync f async k = async (f >=> k)