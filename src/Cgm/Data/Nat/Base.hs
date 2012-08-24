{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeFamilies, Rank2Types, FlexibleContexts, GADTs, TypeOperators, DeriveDataTypeable #-}

module Cgm.Data.Nat.Base (
    D0,
    d0,
    Nat(..),
    Succ,
    intOfNat,
    (:+:),
    addZ,
    addS,
    addSC,
    addC,
    addComm,
    (:-:),
    subZ,
    subS,
    subI,
    addSub,
    geTrans,
    module Cgm.Data.Tagged
) where

import Control.Applicative
import Cgm.Data.Tagged
import Data.Typeable

data D0 = D0 deriving Typeable
newtype Succ n = Succ n deriving Typeable

d0 = D0

-- Similar to http://www.mail-archive.com/haskell-cafe@haskell.org/msg60806.html

class Nat n where onNat :: (n ~ D0 => c) -> (forall n'. (Succ n' ~ n, Nat n') => Tagged n' c) -> Tagged n c
instance Nat D0 where onNat z _ = Tagged z
instance Nat n => Nat (Succ n) where onNat _ s = Tagged $ untag s

intOfNat :: Nat a => Tagged a Int
intOfNat = onNat 0 $ (+ 1) <$> intOfNat

on2Nats :: (Nat a, Nat b) => 
           ((a ~ D0, b ~ D0) => z) -> 
           (forall b'. (a ~ D0, Succ b' ~ b, Nat b') => Tagged b' z) -> 
           (forall a'. (Succ a' ~ a, Nat a', b ~ D0) => Tagged a' z) -> 
           (forall a' b'. (Succ a' ~ a, Nat a', Succ b' ~ b, Nat b') => Tagged2 a' b' z) -> 
           Tagged2 a b z
on2Nats zz zs sz ss = onNat (onNat zz zs) $ flipTags $ onNat sz $ flipTags ss

--instance Show (Nat a) where
--  showsPrec _ D0 = showString "d0"
--  showsPrec p a@(S n) = if (natToInt a <= 65)
--                        then showString ("d" ++ show (natToInt a))
--                        else showParen (p > 10) $ showString "S" . showChar ' ' . showsPrec 11 n

--

--type family Nest n (f :: * -> *) a
--type instance Nest D0 f a = a
--type instance Nest (Nat (Succ n)) f a = f (Nest n f a)

--nest :: Nat n -> (forall a. a -> f a) -> a -> Nest n f a
--nest D0 f a = a
--nest (S n) f a = f (nest n f a)

-- Add

type family a :+: b
type instance D0 :+: D0 = D0
type instance D0 :+: Succ b = Succ b
type instance Succ a :+: D0 = Succ a
type instance Succ a :+: Succ b = Succ (Succ (a :+: b))

addZ :: Nat b =>  (((D0 :+: b) ~ b, (b :+: D0) ~ b) => c) -> Tagged b c
addZ c = onNat c $ tag c

addS :: forall a b z. (Nat a, Nat b) => ((Succ a :+: b) ~ Succ (a :+: b) => z) -> Tagged2 a b z
addS z = on2Nats z (addZ z) (tag z) $ addS z
--This is what the above definition looked like before we used on2Nats:
--addS c = onNat (onNat c $ addZ c) (flipTags $ onNat (tag c) $ flipTags $ addS c)

addSC :: forall a b z. (Nat a, Nat b) => (((Succ a :+: b) ~ Succ (a :+: b), Nat (a :+: b)) => z) -> Tagged2 a b z
addSC z = on2Nats z (addZ z) (tag z) $ addSC z

addSR :: forall a b z. (Nat a, Nat b) => ((a :+: Succ b) ~ Succ (a :+: b) => z) -> Tagged2 a b z
addSR z = on2Nats z (tag z) (addZ z) $ addSR z

addComm :: forall a b z. (Nat a, Nat b) => ((a :+: b) ~ (b :+: a) => z) -> Tagged2 a b z
addComm z = on2Nats z (tag z) (tag z) $ addComm z

addC :: forall a b z. (Nat a, Nat b) => (Nat (a :+: b) => z) -> Tagged2 a b z
addC z = on2Nats z (tag z) (tag z) $ addC z

-- Subtract

-- We use only the canonical representation: Succ (Pred a) is never used, and neither is Pred (Succ a)
newtype Pred n = Pred n

type family a :-: b
type instance D0 :-: D0 = D0
type instance D0 :-: Succ a = Pred a
type instance Succ a :-: D0 = Succ a
type instance Succ a :-: Succ b = a :-: b

newtype Neg a = Neg a

-- Usatisfiable context, would prove any z
--zSubS :: forall a z. (Nat a, Nat (D0 :-: Succ a)) => (D0 ~ Succ D0 => z) -> Tagged a z
--zSubS z = (at :: At (D0 :-: Succ a)) $ onNat (onNat z (tag z)) (flipTags $ onNat (tag z) (flipTags $ zSubS z))

-- GHC produces an error on dead code, instead of not checking it. Not checking at all might be confusing
-- when the code was not expected by the programmer to be dead. So a warning seems appropriate (but there should be no type errors,
-- no matter what the code contains). When the programmer expects the code to be dead, it should be possible
-- to write dead code which does not produce the warning. One solution would be to warn only when the dead code
-- calls non-dead code (more precisely, code that the compiler cannot determine to be dead based on its incomplete checks.)
-- This way a special function dead :: Bool ~ Int => a could be called to disable the warning, but recursive calls to the dead-code would
-- also not generate any warning. Using undefined instead of dead would no be satisfactory, since undefined may be used
-- anywhere including in non-dead code, without producing any error.
-- As a temporary solution, we define badNat :: Nat (Pred D0) => z, which is similar to undefined, but requires a proof
-- that we are in quasi-dead code, code which violates the invariants on Nat, and which may legally call an intOfNat which
-- is undefined. If we had the treatment discussed above for dead-code, we could replace uses of this by onNat dead dead
badNat :: Nat (Pred a) => Tagged (Pred a) z
badNat = undefined -- equavalient to (intOfNat `seq` undefined)

subZ :: Nat b =>  ((b :-: D0) ~ b => z) -> Tagged b z
subZ z = onNat z $ Tagged z

subI :: Nat b =>  ((b :-: b) ~ D0 => z) -> Tagged b z
subI z = onNat z $ subI z

subS :: forall a b z. (Nat a, Nat b, Nat (a :-: b)) => ((Succ a :-: b) ~ Succ (a :-: b) => z) -> Tagged2 a b z
subS z = on2Nats z (tag $ (at :: At (a :-: b)) badNat) (tag z) $ subS z

addSubI :: forall a b z. (Nat a, Nat b) => (((a :+: b) :-:  b) ~ a => z) -> Tagged2 a b z
addSubI z = on2Nats z (dupTag $ addZ $ (at :: At D0) $ addSubI z)
           (tag z) $ dup2Tags $ addS $ tag $ (at :: At a) $ addSubI z

addSub :: forall a b c z. (Nat a, Nat b, Nat c, Nat (b :-: c)) => ((((a :+: b) :-: c) ~ (a :+: (b :-: c)), Nat (a :+: (b :-: c))) => z) -> Tagged3 a b c z
addSub z = onNat ((at2 :: At2 b (b :-: c)) $ addZ $ addZ $ tag2 z) addSubS where
  addSubS :: forall a'. (Nat a', a ~ Succ a') => Tagged3 a' b c z
  addSubS = addSub $ 
             (at2 :: At2 a' (b :-: c)) $ addSC $
             (at2 :: At2 a' b) $ addSC $
             (at2 :: At2 (a' :+: b) c) $ subS z

--subRS :: forall a b z. (Nat a, Nat (b :-: Succ a)) => (Succ (b :-: Succ a) ~ (b :-: a) => z) -> Tagged2 a b z

geTrans :: forall a b c z. (Nat a, Nat b, Nat c, Nat (a :-: b), Nat (b :-: c)) => (Nat (a :-: c) => z) -> Tagged3 a b c z
geTrans = undefined



-- Notes
-- We have no true kind system, so polymorphic data types may be instantiated at any type, whether it makes sense or not.
-- However we can express the constraint on types as type equality constraints and/or type classes, to be attached
-- to all methods involving the type.
-- These constrains rely on type classes and type families, which are always open. We can close them with
-- the NatArg/IsNat trick below, but this just closes them to a single type constructor, in which case
-- the type family could have been defined on the argument of the constructor.
--type family NatArg a
--type instance NatArg (Nat a) = a
--class Nat (NatArg a) ~ a => IsNat a
--instance IsNat (Nat a)

