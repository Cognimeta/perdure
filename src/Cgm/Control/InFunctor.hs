{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, TypeOperators #-}

module Cgm.Control.InFunctor (
    Function(..),
    ($*),

    Increasing(..),
    Increasing',
    (:>>=),
    uncheckedIncreasing,
    
    StrictlyIncreasing(..),
    StrictlyIncreasing',
    (:>>),
    uncheckedStrictlyIncreasing,
    
    InjectionA(..),
    InjectionA',
    uncheckedInjectionA,
    injectionA',

    Injection(..),
    Injection',
    uncheckedInjection,
    injection',
    pairInjection,

    InjectionM(..),
    InjectionM',
    uncheckedInjectionM,
    injectionM',

    Bijection(..),
    Bijection',
    uncheckedBijection,
    pairBijection,
    inv,
    liftAB,

    InjectionACofunctor(..),
    InjectionCofunctor(..),
    InjectionMCofunctor(..),
    ExpFunctor(..),
    IncreasingFunctor(..),
    StrictlyIncreasingFunctor(..),
    
    Cofunctor(..),
    functorCofunctorComap,
    Comonoidal(..),
    functorIacomap,
    cofunctorIacomap,
    RevFun(..),
    (:<-),
    
    wrapU,
    wrapB,
    liftItI,
    liftIItK,
    liftIItI,
    liftIKtI,
    liftIItII
) where

import Prelude (Ord, (>>=), ($), uncurry)
import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Data.Functor
import Control.Applicative
import Data.Functor.Constant
import Data.Functor.Compose
import Cgm.Control.Combinators

-- Function
-- Laws: f is total

class Category f => Function f where
    apply :: f a b -> a -> b

infixr 0 $*
{-# INLINE ($*) #-}
($*) :: Function f => f a b -> a -> b
($*) = apply

instance Function (->) where 
  {-# INLINE apply #-}
  apply = id

-- Increasing
-- Additional laws: forall a b. a > b implies f a >= f b
class Function f => Increasing f

newtype Increasing' a b = Increasing' (a -> b) -- could be generalized wrap any f in Function instead of just ->
type a :>>= b = Increasing' a b
uncheckedIncreasing :: (a -> b) -> Increasing' a b
uncheckedIncreasing = Increasing'
instance Category Increasing' where
  id = Increasing' id
  (Increasing' a) . (Increasing' b) = Increasing' $ a . b
instance Function Increasing' where apply (Increasing' f) = f
instance Increasing Increasing'

-- Note: assuming total order, strictly increasing implies Injective, we cold reflect that here

-- StrictlyIncreasing
-- Additional laws: forall a b. a > b implies f a > f b
class Increasing f => StrictlyIncreasing f

newtype StrictlyIncreasing' a b = StrictlyIncreasing' (a -> b)
type a :>> b = StrictlyIncreasing' a b
uncheckedStrictlyIncreasing :: (a -> b) -> StrictlyIncreasing' a b
uncheckedStrictlyIncreasing = StrictlyIncreasing'
instance Category StrictlyIncreasing' where
  id = StrictlyIncreasing' id
  (StrictlyIncreasing' a) . (StrictlyIncreasing' b) = StrictlyIncreasing' $ a . b
instance Function StrictlyIncreasing' where apply (StrictlyIncreasing' f) = f
instance Increasing StrictlyIncreasing'
instance StrictlyIncreasing StrictlyIncreasing'

-- InjectionA
-- Additions laws: "fromJust . unapply f . apply f = id", and unapply is total
-- This leaves unspecified the behavior of unapply on values outside the image of apply.

class Function f => InjectionA f where
    unapply :: f a b -> b -> Maybe a

data InjectionA' a b = InjectionA' !(a -> b) !(b -> Maybe a)
instance Category InjectionA' where
  id = InjectionA' id Just
  f . g = InjectionA' (apply f . apply g) ((>>= unapply g) . unapply f)
instance Function InjectionA' where apply (InjectionA' f _) = f
instance InjectionA InjectionA' where unapply (InjectionA' _ f') = f'

uncheckedInjectionA :: (a -> b) -> (b -> Maybe a) -> InjectionA' a b
uncheckedInjectionA = InjectionA'

injectionA' :: InjectionA f => f a b -> InjectionA' a b
injectionA' f = uncheckedInjectionA (apply f) (unapply f)

-- Injection
-- Additions laws: retract.apply = id
-- This leaves unspecified the behavior of unapply on values outside the image of apply.

class InjectionA f => Injection f where
    retract :: f a b -> b -> a

data Injection' a b = Injection' !(a -> b) !(b -> a)
instance Category Injection' where
  id = Injection' id id
  f . g = Injection' (apply f . apply g) (retract g . retract f)
instance Function Injection' where apply (Injection' f _) = f
instance InjectionA Injection' where unapply (Injection' _ f') = Just . f'
instance Injection Injection' where retract (Injection' _ f') = f'

uncheckedInjection :: (a-> b) -> (b -> a) -> Injection' a b
uncheckedInjection = Injection'

injection' :: Injection f => f a b -> Injection' a b
injection' f = uncheckedInjection (apply f) (retract f)

pairInjection :: (Injection f1, Injection f2) => f1 a1 b1 -> f2 a2 b2 -> Injection' (a1, a2) (b1, b2)
pairInjection f1 f2 = uncheckedInjection (apply f1 *** apply f2) (retract f1 *** retract f2)

-- InjectionM
-- Additions laws: "unapply f b = Nothing" for any b such that forall a, "apply f a \= b"
class InjectionA f => InjectionM f

newtype InjectionM' a b = InjectionM' (InjectionA' a b) deriving (Category, Function, InjectionA)
instance InjectionM InjectionM'

uncheckedInjectionM :: (a-> b) -> (b -> Maybe a) -> InjectionM' a b
uncheckedInjectionM = InjectionM' ./ uncheckedInjectionA

injectionM' :: InjectionM f => f a b -> InjectionM' a b
injectionM' f = uncheckedInjectionM (apply f) (unapply f)


-- Bijection
-- Laws: "apply f . retract f = id", and the laws of the superclasses

class (Injection f, InjectionM f) => Bijection f

newtype Bijection' a b = Bijection' (Injection' a b) deriving (Category, Function, InjectionA, Injection)
instance InjectionM Bijection'
instance Bijection Bijection'

uncheckedBijection :: (a -> b) -> (b -> a) -> Bijection' a b
uncheckedBijection = Bijection' ./ uncheckedInjection

pairBijection :: (Bijection f1, Bijection f2) => f1 a1 b1 -> f2 a2 b2 -> Bijection' (a1, a2) (b1, b2)
pairBijection = Bijection' ./ pairInjection

inv :: Bijection f => f a b -> Bijection' b a
inv f = uncheckedBijection (retract f) (apply f)

liftAB :: (Bijection g, Applicative f) => g a b -> Bijection' (f a) (f b)
liftAB g = uncheckedBijection (apply g <$>) (retract g <$>)

--

class InjectionACofunctor f where iacomap :: InjectionA g => f b -> g a b -> f a
class InjectionCofunctor f where icomap :: Injection g => f b -> g a b -> f a
class InjectionMCofunctor f where imcomap :: InjectionM g => f b -> g a b -> f a

class ExpFunctor f where bmap :: Bijection g => g a b -> f a -> f b

class IncreasingFunctor f where incmap :: Increasing g => g a b -> f a -> f b
class StrictlyIncreasingFunctor f where sincmap :: StrictlyIncreasing g => g a b -> f a -> f b
  
-----
                                        
instance (Applicative a, InjectionACofunctor c) => InjectionACofunctor (Compose a c) where  
  iacomap (Compose c) i = Compose $ iacomap <$> c <*> pure i
                                        
-----
                                        
class Cofunctor f where
  infixr 4 >$<
  (>$<) :: f b -> (a -> b) -> f a
  
newtype RevFun b a = RevFun {getRevFun :: a -> b} deriving Monoid
type (:<-) = RevFun -- until deriveStructure can accept a type operator
instance Cofunctor (RevFun b) where 
  {-# INLINE (>$<) #-}
  (RevFun g) >$< f = RevFun $ g . f
instance Cofunctor (Constant a) where 
  {-# INLINE (>$<) #-}
  (Constant a) >$< f = Constant a                                   
instance (Functor f, Cofunctor c) => Cofunctor (Compose f c) where 
  {-# INLINE (>$<) #-}
  (>$<) = functorCofunctorComap -- TODO: cleanup: There is an alternative defintion so a newtype would be required to indicate which is desired
  
functorCofunctorComap :: (Functor f, Cofunctor c) => (Compose f c) b -> (a -> b) -> (Compose f c) a
functorCofunctorComap (Compose c) abf = Compose $ (>$< abf) <$> c

-- See definition of Monoidal in http://strictlypositive.org/IdiomLite.pdf
-- If the superclass is changed to Cofunctor (or an exponential functor), it seems the Monoidal laws can be adapted.
-- The munit and mpair methods here would be the special case where the cofunctor is (_ -> m) for some monoid m.
-- Maybe we could move the operations to a superclass with no laws, and have empty Monoidal/Comonoidal classes
-- that derive it and just introduce the laws.
class Comonoidal f where  
  munit :: f ()
  mpair :: f a -> f b -> f (a, b)
  
instance Monoid m => Comonoidal (RevFun m) where
  munit = mempty
  mpair (RevFun fa) (RevFun fb) = RevFun $ uncurry $ mappend `dot2` fa $ fb
instance Monoid m => Comonoidal (Constant m) where
  munit = Constant mempty
  mpair (Constant fa) (Constant fb) = Constant $ mappend fa fb
instance (Applicative a, Comonoidal c) => Comonoidal (Compose a c) where
  munit = Compose $ pure munit
  mpair (Compose a) (Compose b) = Compose $ mpair <$> a <*> b

-- TODO rename to unsafe
{-# INLINE functorIacomap #-}
functorIacomap :: (Functor f, InjectionA g) => f b -> g a b ->  f a
functorIacomap f g = (fromJust . unapply g) <$> f
{-# INLINE cofunctorIacomap #-}
cofunctorIacomap :: (Cofunctor f, InjectionA g) => f b -> g a b ->  f a
cofunctorIacomap f g = f >$< apply g

-----

wrapU :: (Bijection og, Function i1g) => og o o' -> i1g i1 i1' -> (i1' -> o') -> i1 -> o
wrapU o i1 f = retract o . f . (i1 $*)
wrapB :: (Bijection og, Function i1g, Function i2g) => og o o' -> i1g i1 i1' -> i2g i2 i2' -> (i1' -> i2' -> o') -> i1 -> i2 -> o
wrapB o i1 i2 f = retract o ./ dot2 f (i1 $*) (i2 $*)

-- We cannot express sufficiently general type. Those functions are rarely useful.
liftItI b f = retract b . f . apply b
liftIItK b f = dot2i f (apply b)
liftIItI b f = retract b ./ liftIItK b f
liftIKtI b f = retract b ./ dot2 f (apply b) id
liftIItII b f = (retract b *** retract b) ./ liftIItK b f

