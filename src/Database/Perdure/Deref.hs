{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE GADTs, ScopedTypeVariables #-}


module Database.Perdure.Deref (
  Deref(..),
  deref,
  derefEq
  ) where

import System.IO.Unsafe
import Cgm.Data.Functor.Sum
import Cgm.Data.Maybe
import Cgm.Control.Combinators
import Database.Perdure.Persistent
import Database.Perdure.CDeserializer
import Database.Perdure.WordArrayRef()
import Database.Perdure.WordNArrayRef
import Control.Applicative
import Data.Word
import Control.Concurrent.MVar
import Control.Exception.Base
import qualified Database.Perdure.Cache as Cache
import Data.Dynamic
import Control.Monad.Random
import Debug.Trace

class Deref r where
  derefIO :: r a -> IO a

{-# NOINLINE deref #-}
deref :: Deref r => r a -> a
deref = unsafePerformIO . derefIO

derefEq :: (Deref r, Eq a) => r a -> r a -> Bool
derefEq = (==) `dot2i` deref

instance Deref DRef where 
  derefIO dref@(DRef p dc@(DeserializerContext f cv) aRef) = 
    let addr = arrayRefAddr aRef
    in --trace ("looking up cache at" ++ show addr)
     modifyMVar cv (\c -> return $ (maybe (c, Nothing) $ \(e, c') -> (c', Just $ Cache.entryValue e)) $ Cache.lookup addr c) >>= 
     maybe 
     ((>>= \a -> evaluate a >>
                 (a <$ modifyMVar_ cv (evalRandIO . trace ("adding to cache at" ++ show addr) .
                                       Cache.insert addr (Cache.Entry (toDyn a) $ arrayRefSize aRef)))) $
      fmap (maybe (error "Read error") $ deserializeFromFullArray (cDeser p dc) . (id :: Id (ArrayRange (PrimArray Free Word)))) $
      (fmap $ trace $ "Deserializing array from " ++ showDRef dref) $
      derefArrayRef f aRef)
     (return . fromMaybe (error $ "Wrong type in cache cell " ++ show addr) . fromDynamic)

instance Show a => Show (DRef a) where show = show . deref
instance Eq a => Eq (DRef a) where (==) = derefEq

instance (Deref ra, Deref rb) => Deref (Sum ra rb) where derefIO = either derefIO derefIO . getSum

instance Deref r => Deref (IRef r) where derefIO = derefIO . getIRef
instance (Deref r, Eq t) => Eq (IRef r t) where (==) = (==) `dot2i` deref
instance (Deref r, Show t) => Show (IRef r t) where show = show . deref . getIRef

instance Deref Ref0 where derefIO (Ref0 a) = return a
instance Show a => Show (Ref0 a) where show = show . deref

instance Deref r => Deref (CRef r) where derefIO = onCRef derefIO return
instance (Deref r, Show a) => Show (CRef r a) where show = show . deref
instance (Deref r, Eq a) => Eq (CRef r a) where (==) = derefEq
