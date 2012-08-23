{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}


{-# LANGUAGE Rank2Types #-}

module Cgm.Data.Tagged (
    module Data.Tagged,
    tag,
    at,
    At,
    Tagged2,
    tag2,
    at2,
    At2,
    Tagged3,
    tag3,
    at3,
    At3,
    Tagged4,
    tag4,
    at4,
    At4,
    flipTags,
    rot3Tags,
    rot3Tags2,
    rot4Tags,
    rot4Tags2,
    rot4Tags3,
    dupTag,
    dup2Tags,
    pushTagIn,
    constAt
) where

import Data.Tagged
import Control.Applicative
import Cgm.Control.Combinators

type At a = forall z. Tagged a z -> z
at = untag

type Tag a = forall z. z -> Tagged a z
tag = Tagged

type Tagged2 a b z = Tagged a (Tagged b z)
tag2 = tag . tag
type At2 a b = forall z. Tagged2 a b z -> z
at2 = at . at

type Tagged3 a b c z = Tagged a (Tagged2 b c z)
tag3 = tag . tag2
type At3 a b c = forall z. Tagged3 a b c z -> z
at3 = at . at . at

type Tagged4 a b c d z = Tagged a (Tagged3 b c d z)
tag4 = tag . tag3
type At4 a b c d = forall z. Tagged4 a b c d z -> z
at4 = at . at . at . at

flipTags :: Tagged2 a b z -> Tagged2 b a z
flipTags = tag2 . at2

rot3Tags :: Tagged3 b c a z -> Tagged3 a b c z
rot3Tags = tag3 . at3
rot3Tags2 :: Tagged3 c a b z -> Tagged3 a b c z
rot3Tags2 = tag3 . at3

rot4Tags :: Tagged4 b c d a z -> Tagged4 a b c d z
rot4Tags = tag4 . at4
rot4Tags2 :: Tagged4 c d a b z -> Tagged4 a b c d z
rot4Tags2 = tag4 . at4
rot4Tags3 :: Tagged4 d a b c z -> Tagged4 a b c d z
rot4Tags3 = tag4 . at4

dupTag :: Tagged2 a a z -> Tagged a z
dupTag = tag . at2

dup2Tags :: Tagged4 a b a b z -> Tagged2 a b z
dup2Tags = tag2 . at4

pushTagIn :: Functor f => Tagged a (f b) -> f (Tagged a b)
pushTagIn = (tag <$>) . untag

constAt :: Tagged b a -> b -> a
constAt = at ./ const