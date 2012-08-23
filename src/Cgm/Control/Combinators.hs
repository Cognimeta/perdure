{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Control.Combinators (
    fk,
    dot2,
    dot2i,
    dup,
    (./),
    (.//),
    (.///),
    (.////),
    Id,
    todo
) where



-- A combinator like Generics.Pointless.Combinators./\ but generalized from (,) to any binary function.
-- T
-- /\ = fk (,)
{-# INLINE fk #-}
fk :: (x -> y -> b) -> (a -> x) -> (a -> y) -> a -> b
fk b x y a = b (x a) (y a)
-- fk b x y = dup (dot2 b x y)

-- Like '.' but takes two unary functions on the right, and composes them with a binary
-- functions on the left. May be used infix as in: ++ `dot2` show $ show
{-# INLINE dot2 #-}
dot2 :: (a' -> b' -> c) -> (a -> a') -> (b -> b') -> a -> b -> c
dot2 c a' b' a b = c (a' a) (b' b)

-- Like '.' but takes a binary function on the left, and a unary function on the right
-- The unary function is applied to both arguments
{-# INLINE dot2i #-}
dot2i :: (a' -> a' -> b) -> (a -> a') -> a -> a -> b
dot2i b = dup $ dot2 b

{-# INLINE dup #-}
dup :: (a -> a -> b) -> a -> b
dup bin a = bin a a

-- Composes a function with a binary function
-- It is called 'dot' at http://www.haskell.org/haskellwiki/Pointfree and is equivalent to (.) . (.)
infixr 9 ./
{-# INLINE (./) #-}
(./) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
f ./ b = (f .) . b

-- Composes a function with a ternary function
infixr 9 .//
{-# INLINE (.//) #-}
(.//) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
f .// t = (f .) ./ t

-- Composes a function with a 4-ary function
infixr 9 .///
{-# INLINE (.///) #-}
(.///) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> c
f ./// q = (f .) .// q

-- Composes a function with a 5-ary function
infixr 9 .////
{-# INLINE (.////) #-}
(.////) :: (b -> c) -> (a1 -> a2 -> a3 -> a4 -> a5 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> c
f .//// q = (f .) ./// q

type Id a = a -> a

todo = undefined

