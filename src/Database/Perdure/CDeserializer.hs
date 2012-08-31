{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, TypeFamilies, RankNTypes, DeriveFunctor, FlexibleContexts, ScopedTypeVariables #-}

module Database.Perdure.CDeserializer (
  cDeser,
  deserializeFromArray,
  deserializeFromFullArray,
  unsafeSeqDeserializer,
  Deserializer(..),
  Deserializable(..),
  DeserOut(..),
  module Database.Perdure.Persistent,
  module Database.Perdure.CRef
  ) where

import Prelude ()
import Cgm.Prelude
import Database.Perdure.Persistent
import Database.Perdure.StoreFile
import Database.Perdure.CRef
import Cgm.Data.Word
import Data.Bits

-- TODO figure out why the Deserializer's Allocator df is free to differ from f. Why is it a type argument of Deserializer at all if it can be anything?

-- TODO consider reimplementing as in CSerializer (no Serializer layer, and possibly with continuations) and check performance

deserializeFromArray :: (Allocation f, Allocation df, Deserializable w) => Deserializer df a -> ArrayRange (PrimArray f w) -> DeserOut a
deserializeFromArray d = (\(ArrayRange ar start _) -> deserialize d (refineLen start) ar) . deserInput

deserializeFromFullArray :: forall f df w a. (Allocation f, Allocation df, Deserializable w, LgMultiple w Bool, Prim w) => 
                            Deserializer df a -> ArrayRange (PrimArray f w) -> a
deserializeFromFullArray d ar = case deserializeFromArray d ar of
  DeserOut a end -> bool (error $ "Inconsistent deserialized size: " ++ show (end, refineLen $ arrayLen ar :: Len Bool Word)) a $ 
             (coarsenLen end :: Len w Word) == arrayLen ar

-- | The passed persister must have no references
unsafeSeqDeserializer :: Persister a -> Deserializer Free a
unsafeSeqDeserializer p =
  cDeser p (DeserializerContext (error "seqDeserializer has no file" :: ReplicatedFile) (error "seqDeserializer has no cache"))

cDeser :: Persister a -> DeserializerContext -> Deserializer Free a
cDeser p = case p of
  PartialWordPersister n -> const $ partialWordDeserializer n
  PairPersister pa pb -> liftA2 (liftA2 (,)) (cDeser pa) (cDeser pb)
  EitherPersister pa pb -> \dc -> cDeser persister dc >>= bool (Left <$> cDeser pa dc) (Right <$> cDeser pb dc)
  ViewPersister i pb -> flip functorIacomap i . cDeser pb
  SummationPersister pi' d _ -> \dc -> cDeser pi' dc >>= d (\pb ba -> fmap ba $ cDeser pb dc)
  DRefPersister' -> \dc -> DRef persister dc <$> cDeser persister dc 
  CRefPersister' _ pra -> fmap Refed . cDeser pra
  
instance InjectionACofunctor (Deserializer f) where
  {-# INLINE iacomap #-}
  iacomap = functorIacomap

bitDeserializer :: Deserializer f Word
bitDeserializer = Deserializer $ \b ar -> DeserOut (let (wIx, bIx) = coarseRem b in (indexArray ar wIx `partialShiftRL` getLen bIx) .&. 1) (b + 1)

-- 0 <= n <= wordBits
partialWordDeserializer :: Len Bool Word -> Deserializer f Word
partialWordDeserializer n
  | n == 0 = pure 0
  | n == 1 = bitDeserializer
  | otherwise =
    Deserializer $ \b ar -> 
    DeserOut (
      let (wIx, bIx) = coarseRem b
          avail = wordBits - bIx
          overflow = n - avail
          n' = wordBits - n
      in bool (indexArray ar wIx `partialShiftL` getLen (n' - bIx) `partialShiftRL` getLen n') 
         ((indexArray ar wIx `partialShiftRL` getLen bIx) +
          (indexArray ar (wIx + 1) `partialShiftL` getLen (wordBits - overflow) `partialShiftRL` getLen n')) $ 
         (signed $* getLen overflow) > 0)
    (b + n)

---------------------------------------------------------------------------

class Deserializable a where deserInput :: (Allocation f, Allocation f') => ArrayRange (PrimArray f a) -> ArrayRange (PrimArray f' Word)
  
instance Deserializable Word where deserInput = primArrayMatchAllocation
instance Deserializable Word32 where
  deserInput a = onWordConv
                 (primArrayMatchAllocation $ retract wordConvArrayRange a) 
                 (fullArrayRange $ mkArrayWith (coarsenLen $ arrayLen a) $ 
                        (\i -> retract wordConv $ retract splitWord64LE 
                               (indexArray a i, if i + 1 < arrayLen a then indexArray a (i + 1) else 0)) . refineLen) -- TODO: test
instance Deserializable Word64 where
  deserInput a = onWordConv
                 (fullArrayRange $ mkArrayWith (refineLen $ arrayLen a) $ 
                        (\(i, r) -> retract wordConv $ bool fst snd (r == 0) $ apply splitWord64LE $ indexArray a i) . coarseRem) -- TODO test
                 (primArrayMatchAllocation $ retract wordConvArrayRange a) 

---------------------------------------------------------------------------

-- Deserialization is strict so once DeserOut is forced, we no longer retain the input array. 
-- The strict annotation in DeserOut is not quite sufficient to enfoce that, we also have to make sure we do not put in
-- there a structure with interal thunks pointing to the array. But inspecting this file should show no such Deserializer.
data DeserOut a = DeserOut {
  deserValue :: !a,
  deserPos :: {-# UNPACK #-} !(Len Bool Word)} deriving Functor

-- If deserialization was lazy, the lifetime of the array would be unpredictable, so to avoid fragmentation of pinned arrays, we would prefer f = Free
-- But now deserialization is strict and the array is not retained so either Free or Pinned will do.
newtype Deserializer f a = Deserializer {deserialize :: Len Bool Word -> PrimArray f Word -> DeserOut a}
instance Functor (Deserializer f) where
  {-# INLINE fmap #-}
  fmap g = Deserializer . ((fmap g <$>) <$>) . deserialize
--instance Pointed (Deserializer c) where
--    point x = ... inlined into pure
instance Applicative (Deserializer f) where
  {-# INLINE pure #-}
  pure x = Deserializer $ \b _ -> DeserOut x b
  {-# INLINE (<*>) #-}
  g <*> x = Deserializer $ \b ptr -> case deserialize g b ptr of DeserOut g' b' -> deserialize (g' <$> x) b' ptr
    -- we could define <*> = ap, but we would like to see if we could use <*> in the definition of join instead, see reflexion below
instance Monad (Deserializer f) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) = fmap join' . flip fmap where
    {-# INLINE join' #-}
    join' :: Deserializer f (Deserializer f a) -> Deserializer f a
    join' d2 = Deserializer $ \b ptr -> case deserialize d2 b ptr of DeserOut d b' -> deserialize d b' ptr
