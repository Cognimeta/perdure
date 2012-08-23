{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

-----------------------------------------------------------------------------
--
-- Module      :  Maybe
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Cgm.Data.Maybe (
  module Data.Maybe,
  ifJust,
  justIf,
  predJust,
  firstJust,
  maybeToEither
) where

import Data.Monoid
import Data.Maybe
import Cgm.Control.Combinators
import Cgm.Data.Bool

ifJust :: Bool -> a -> Maybe a
ifJust = bool (const Nothing) Just

justIf :: a -> Bool -> Maybe a
justIf = flip ifJust

predJust :: (a -> Bool) -> a -> Maybe a
predJust p a = p a `ifJust` a

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust = getFirst ./ dot2i mappend First

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b = maybe (Left b) Right