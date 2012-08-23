{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Data.Monoid(
  (◊),
  ø,
  inter,
  foldInter,
  module Data.Monoid
  ) where

import Data.Monoid
import Data.Foldable
import Data.List

-- Right associativity helps with performance, on [] or on Data.Text.Lazy.Builder for example
infixr 9 ◊ -- rfc1345="&LZ"
(◊) :: Monoid a => a -> a -> a
(◊) = mappend

-- Unicode (LATIN SMALL LETTER O WITH STROKE) rfc1345="&o/"
ø :: Monoid m => m
ø = mempty

inter :: Monoid m => m -> m -> m -> m
inter i a b = a ◊ i ◊ b

foldInter :: (Monoid m, Foldable l) => m -> l m -> m
foldInter i l = fold $ intersperse i $ toList l
