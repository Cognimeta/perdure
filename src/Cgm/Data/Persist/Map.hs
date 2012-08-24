{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, KindSignatures, TypeFamilies, FlexibleContexts #-}

module Cgm.Data.Persist.Map(
  Map,
  updateM,
  empty,
  null,
  lookup,
  insert,
  insertWith,
  delete,
  foldlWithKey,
  foldrWithKey,
  fromList,
  toList,
  assocs,
  elems,
  maxKey,
  scan,
  Cgm.Data.Persist.Map.mapLens
  ) where

import Prelude hiding (null, lookup)
import Cgm.Control.Monad.State
import Database.Perdure.Persistent
import Database.Perdure.SizeRef
import Control.Arrow
import Control.Monad
import Data.Tuple
import Data.Functor
import Data.List(foldl')
import Data.Lens
import Cgm.Data.Nat
import Database.Perdure.Ref
import Cgm.Data.Maybe
import Control.Comonad.Trans.Store
import Data.Dynamic
import Database.Perdure.Package
import Cgm.Data.Typeable

type R = CRef (SizeRef D9)

moduleName :: String
moduleName = "Cgm.Data.Persist.Map"

-- | Unlike Data.Map, this Map does not support constant time size computation
data Map k a = Empty | NonEmpty !(Tree Leaf k a) deriving (Functor, Typeable)
instance (Show k, Show a) => Show (Map k a) where show = show . toList

newtype Reference t k a = Reference (R (t k a)) deriving Functor
instance Typeable2 t => Typeable2 (Reference t) where 
  typeOf2 _ = mkTyCon3 perdurePackage moduleName "Reference" `mkTyConApp` [typeOf2 (undefined :: t () ())]
dereference :: Reference t k a -> t k a
dereference (Reference t) = deref t
reference :: t k a -> Reference t k a
reference = Reference . ref
newtype Leaf k a = Leaf a deriving (Functor, Typeable) -- TODO consider using data would allow's map's values to be thunks, as in Data.Map
data Empt k a = Empt

data Tree t k a = LastLevel !(Upper t k a) | NextLevel !(Tree (Reference (Node t)) k a) deriving (Functor)

-- The k's are the maximum elements of the preceeding 'c'. The maximum element of the last 'c' is not stored but is passed from the context.
data Node t k a = 
  Node2 {-# UNPACK #-} !(Upper t k a) !(t k a) | 
  Node3 {-# UNPACK #-} !(Upper t k a) {-# UNPACK #-} !(Upper t k a) !(t k a) deriving (Functor)
instance Typeable2 t => Typeable2 (Node t) where 
  typeOf2 _ = mkTyCon3 perdurePackage moduleName "Node" `mkTyConApp` [typeOf2 (undefined :: t () ())]

data Upper t k a = Upper !k !(t k a) deriving (Functor) -- The k is the largest key in (t k a)
mapUpper :: (ta k a -> tb k b) -> Upper ta k a -> Upper tb k b
mapUpper f (Upper k tka) = Upper k $ f tka
within :: Ord k => Upper t k a -> k -> Bool
within (Upper k0 _) k = k <= k0

node2 :: Upper t k a -> Upper t k a -> Upper (Node t) k a
node2 uc0 (Upper u1 c1) = Upper u1 $ Node2 uc0 c1
node3 :: Upper t k a -> Upper t k a -> Upper t k a -> Upper (Node t) k a
node3 uc0 uc1 (Upper u2 c2) = Upper u2 $ Node3 uc0 uc1 c2
onNode :: (Upper t k a -> Upper t k a -> z) -> (Upper t k a -> Upper t k a -> Upper t k a -> z) -> Upper (Node t) k a -> z
onNode z2 z3 (Upper u n) = case n of
  Node2 uc0 c1 -> z2 uc0 $ Upper u c1
  Node3 uc0 uc1 c2 -> z3 uc0 uc1 $ Upper u c2

data Out g c =
  Merge g |
  Single c |
  Split c c

appState :: Monad m => a -> (a -> b) -> StateT a m z -> StateT b m z
appState a ab = viewState (const a, ab)

-- | General update/lookup at a single key. The State monad used supports
-- non-modification. Nothing denotes the absence of a value.
updateM :: forall k a b. Ord k => k -> State (Maybe a) b -> State (Map k a) b
updateM k s = get >>= \p -> case p of
  Empty -> appState Nothing (maybe Empty $ NonEmpty . LastLevel . Upper k . Leaf) s
  r@(NonEmpty t) -> (\(c, b) -> onModify (return b) ((b <$) . put) c) $ case t of
    LastLevel ua -> first (fmap $ either NonEmpty (\Empt -> Empty)) $ trans' k s ua
    NextLevel t -> first (fmap $ NonEmpty . either NextLevel id) $ updateT t where
      updateT :: Tr t => Tree (Reference (Node t)) k a -> (Modify (Either (Tree (Reference (Node t)) k a) (Tree t k a)), b)
      updateT (LastLevel ua) = first (fmap $ either Left (Right . LastLevel)) $ trans' k s ua
      updateT (NextLevel t) = first (fmap $ Left . either NextLevel id) $ updateT t
    
trans' :: (Tr t, Ord k) => k -> State (Maybe a) b -> Upper t k a -> (Modify (Either (Tree t k a) (G t k a)), b)
trans' k s a = first (fmap $ \o -> case o of 
                  Merge a -> Right a
                  Single a -> Left $ LastLevel a
                  Split a0 a1 -> Left $ NextLevel $ LastLevel $ mapUpper reference $ node2 a0 a1
              ) $ trans k s a

data Modify a = Leave | Change !a deriving Functor
onModify :: z -> (a -> z) -> Modify a -> z
onModify l _ Leave = l
onModify _ c (Change a) = c a

class Tr (t :: * -> * -> *) where
  type G t :: * -> * -> *
  trans :: Ord k => k -> State (Maybe a) b -> Upper t k a -> (Modify (Out (G t k a) (Upper t k a)), b)
  mergeLeft :: G t k a -> Upper t k a -> Either (Upper t k a) (Upper t k a, Upper t k a)
  mergeRight :: Upper t k a -> G t k a -> Either (Upper t k a) (Upper t k a, Upper t k a)
  foldlK :: (z -> k -> a -> z) -> z -> Upper t k a -> z
  foldrK :: (k -> a -> z -> z) -> z -> Upper t k a -> z
instance Tr Leaf where
  type G Leaf = Empt
  trans k f a@(Upper k0 (Leaf v0)) = 
    if k == k0
    then first (maybe Leave $ Change . maybe (Merge Empt) (Single . Upper k0 . Leaf)) $ swap $ runState f (Just v0)
    else first (maybe Leave $ maybe Leave $ \v -> Change $ (if k > k0 then id else flip) Split a (Upper k $ Leaf v)) $ swap $ runState f Nothing
  mergeLeft Empt = Left
  mergeRight ua Empt = Left ua
  foldlK f z (Upper k (Leaf a)) = f z k a
  foldrK f z (Upper k (Leaf a)) = f k a z
instance Tr t => Tr (Reference t) where
  type G (Reference t) = G t
  trans k f = first (fmap $ \o -> case o of
                        Merge a -> Merge a
                        Single a -> Single $ mapUpper reference a
                        Split a0 a1 -> Split (mapUpper reference a0) (mapUpper reference a1)
                    ) . trans k f . mapUpper dereference
  mergeLeft g = (mapUpper reference +++ mapUpper reference *** mapUpper reference) . mergeLeft g . mapUpper dereference
  mergeRight a g = (mapUpper reference +++ mapUpper reference *** mapUpper reference) $ flip mergeRight g $ mapUpper dereference a
  foldlK f z = foldlK f z . mapUpper dereference
  foldrK f z = foldrK f z . mapUpper dereference
instance Tr t => Tr (Node t) where
  type G (Node t) = Upper t
  trans k f = onNode
            (\c0 c1 -> if within c0 k
                       then first (fmap $ \v -> case v of
                                      Merge l -> either Merge (Single . uncurry node2) $ mergeLeft l c1
                                      Single c0' -> Single $ node2 c0' c1
                                      Split c00 c01 -> Single $ node3 c00 c01 c1
                                  ) $ trans k f c0
                       else first (fmap $ \v -> case v of
                                      Merge l -> either Merge (Single . uncurry node2) $ mergeRight c0 l
                                      Single c1' -> Single $ node2 c0 c1'
                                      Split c10 c11 -> Single $ node3 c0 c10 c11
                                  ) $ trans k f c1
              )
            (\c0 c1 c2 -> if within c0 k
                          then first (fmap $ \v -> case v of
                                      Merge l -> Single $ either (`node2` c2) (\(c0', c1') -> node3 c0' c1' c2) $ mergeLeft l c1
                                      Single c0' -> Single $ node3 c0' c1 c2
                                      Split c00 c01 -> Split (node2 c00 c01) (node2 c1 c2)
                                  ) $ trans k f c0
                          else if within c1 k
                               then first (fmap $ \v -> case v of
                                              Merge l -> Single $ either (node2 c0) (uncurry $ node3 c0) $ mergeLeft l c2
                                              Single c1' -> Single $ node3 c0 c1' c2
                                              Split c10 c11 -> Split (node2 c0 c10) (node2 c11 c2)
                                          ) $ trans k f c1
                               else first (fmap $ \v -> case v of
                                              Merge l -> Single $ either (node2 c0) (uncurry $ node3 c0) $ mergeRight c1 l
                                              Single c2' -> Single $ node3 c0 c1 c2'
                                              Split c20 c21 -> Split (node2 c0 c1) (node2 c20 c21)
                                          ) $ trans k f c2)
  mergeLeft c = onNode
                (\c0 c1 -> Left $ node3 c c0 c1) 
                (\c0 c1 c2 -> Right (node2 c c0, node2 c1 c2))
  mergeRight n c = ($ n) $ onNode
                (\c0 c1 -> Left $ node3 c0 c1 c) 
                (\c0 c1 c2 -> Right (node2 c0 c1, node2 c2 c))
  foldlK f z  = onNode (\n0 n1 -> foldlK f (foldlK f z n0) n1) (\n0 n1 n2-> foldlK f (foldlK f (foldlK f z n0) n1) n2)
  foldrK f z  = onNode (\n0 n1 -> foldrK f (foldrK f z n1) n0) (\n0 n1 n2-> foldrK f (foldrK f (foldrK f z n2) n1) n0)

-- Here we implemented the function without creating Tr methods. Perhaps we should have done the same for foldlK, foldrK.
-- We could even define them in terms of scan now.  
scan :: forall k a z. (k -> a -> z) -> (k -> z -> z -> z) -> Map k a -> Maybe z
scan kaz kzzz m = case m of 
  Empty -> Nothing  
  NonEmpty tr -> Just $ s1 tr $ \(Upper k (Leaf a)) -> kaz k a where
    s1 :: Tree t k a -> (Upper t k a -> z) -> z
    s1 tr utz = case tr of
      LastLevel ut -> utz ut
      NextLevel tr' -> s1 tr' $ \ut -> onNode 
                                       (\u0@(Upper k0 _) u1 -> kzzz k0 (utz u0) (utz u1)) 
                                       (\u0@(Upper k0 _) u1@(Upper k1 _) u2 -> kzzz k0 (utz u0) $ kzzz k1 (utz u1) (utz u2)) $ 
                                       mapUpper dereference ut

empty :: Map k a
empty = Empty

null :: Map k a -> Bool
null Empty = True
null _ = False

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k = fst . runState (updateM k get)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a m = fromMaybe m $ snd $ runState (updateM k $ put $ Just a) m

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k a m = fromMaybe m $ snd $ runState (updateM k $ get >>= put . Just . maybe a (f a)) m

delete :: Ord k => k -> Map k a -> Map k a
delete k m = fromMaybe m $ snd $ runState (updateM k $ get >>= maybe (return ()) (const $ put Nothing)) m

foldlWithKey :: forall z k a. (z -> k -> a -> z) -> z -> Map k a -> z
foldlWithKey f z m = case m of
  Empty -> z
  NonEmpty t -> foldlf z t where
    foldlf :: Tr t => z -> Tree t k a -> z
    foldlf z (LastLevel t) = foldlK f z t
    foldlf z (NextLevel t) = foldlf z t

foldrWithKey :: forall z k a. (k -> a -> z -> z) -> z -> Map k a -> z
foldrWithKey f z m = case m of
  Empty -> z
  NonEmpty t -> foldrf z t where
    foldrf :: Tr t => z -> Tree t k a -> z
    foldrf z (LastLevel t) = foldrK f z t
    foldrf z (NextLevel t) = foldrf z t

fromList :: Ord k => [(k, a)] -> Map k a
fromList = foldl' (\z (k, a) -> insert k a z) empty

toList :: Map k a -> [(k ,a)]
toList = assocs

assocs :: Map k a -> [(k ,a)]
assocs = foldrWithKey (\k a l -> (k, a) : l) []

elems :: Map k a -> [a]
elems = fmap snd . assocs

mapLens :: Ord k => k -> Lens (Map k a) (Maybe a)
mapLens k = Lens $ \m -> store (\mv -> case mv of
    Nothing -> delete k m
    Just v' -> insert k v' m
  ) (lookup k m)

instance (Eq k, Eq a) => Eq (Map k a) where
  a == b = toList a == toList b

maxKey :: Map k a -> Maybe k
maxKey Empty = Nothing
maxKey (NonEmpty t) = Just $ maxKeyT t where
  maxKeyT :: Tree t k a -> k
  maxKeyT (NextLevel t) = maxKeyT t
  maxKeyT (LastLevel (Upper k _)) = k


{-

data Map k a = Empty | NonEmpty (Tree k (Leaf k a))

-- A balanced non-empty tree. There is an arbitrary number or Node nestings, but after ever 4 a CDRef wraps the tree.
-- This means we get nodes in persistent storage that have between 2^4=16 and 3^4=81 children (perhaps on average 2*3*2*3 = 36).
-- A random tree of 10^6 elements would mean 3.86 such persistent storage levels (we simplify by just assming that the probability
-- of Node2 and Node3 is equal).
-- The user can use Tree k a, or Tree k (CDRef a) depending on the size of a.
-- Note that a 10 level tree would be represented as L4 (... L4 (... L2 (... ))), yet the L2 represents the top most, or outer, layers
data Tree k c = 
  L0 !(Upper k c) | 
  L1 !(Upper k (Node k c)) | 
  L2 !(Upper k (Node k (Node k c))) | 
  L3 !(Upper k (Node k (Node k (Node k c)))) |
  L4 !(Tree k (CDRef (Node k (Node k (Node k (Node k c))))))


-}

instance (Persistent a, Persistent k, Typeable k, Typeable a) => Persistent (Map k a) where persister = structureMap persister
instance (Persistent k, Persistent (t k a), Typeable2 t, Typeable k, Typeable a) => Persistent (Tree t k a) where persister = structureMap persister
instance Persistent a => Persistent (Leaf k a) where persister = structureMap persister
instance (Persistent k, Persistent (t k a)) => Persistent (Node t k a) where persister = structureMap persister
instance (Persistent (t k a), Typeable (t k a)) => Persistent (Reference t k a) where persister = structureMap persister
instance (Persistent k, Persistent (t k a)) => Persistent (Upper t k a) where persister = structureMap persister

deriveStructured ''Map
deriveStructured ''Tree
deriveStructured ''Leaf
deriveStructured ''Node
deriveStructured ''Reference
deriveStructured ''Upper
