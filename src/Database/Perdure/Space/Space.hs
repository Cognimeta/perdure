{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Database.Perdure.Space.Space (
  Space(..),
  Span,
  module Cgm.Data.SortedPair,
  module Data.Monoid
  ) where

import Prelude()
import Cgm.Prelude
import Data.Monoid
import Data.Word
import Cgm.Data.SortedPair

type Span = SortedPair Word64

-- Users must ensure they do not add overlapping spans, or remove spans unless all of its contents has already been added (perhaps as seperate spans)
class Space a where
  emptySpace :: a
  removeSpan :: Span -> a -> a
  addSpan :: Span -> a -> a
  findSpan :: Word64 -> a -> [Span]
  isFreeSpace :: Word64 -> a -> Bool

