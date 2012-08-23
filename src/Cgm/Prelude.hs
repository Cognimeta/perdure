{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Cgm.Prelude (
  counting,
  --module Prelude.Plus,
  module Control.Applicative, 
  module Control.Arrow, 
  module Control.Monad, 
  module Control.Exception, 
  module Control.Category, 
  module Data.Traversable, 
  module Data.Foldable, 
  module Data.Monoid, 
  module Data.List, 
  module Data.Function, 
  module Data.Ord, 
  module Prelude, 

  module Cgm.Control.Combinators,
  module Cgm.Control.InFunctor,
  module Cgm.Data.Tagged,
  module Cgm.Data.Bool
  ) where

--import Prelude ()
import Prelude hiding (
    catch, ioError, appendFile, foldr, all, and, any, concat, concatMap, elem, 
    foldl, foldr1, notElem, or, mapM, sequence, mapM_, sequence_, foldl1, 
    maximum, minimum, product, sum, id,(.))
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad hiding (
    mapM,sequence,forM,mapM_,sequence_,forM_,msum,Monad(..),(=<<),Functor(..))
import Data.Traversable
import Data.Foldable
import Data.Monoid hiding (Sum(..)) 
import Data.List hiding (
    foldr,all,and,any,concat,concatMap,elem,foldl,foldr1
  , notElem,or,foldl1,maximum,minimum,product,sum,find,foldl'
  , maximumBy,minimumBy,mapAccumL,mapAccumR,null
  , (++),map,(!!),break,cycle,drop,dropWhile,filter
  , head,init,iterate,last,length,lookup,repeat,replicate
  , reverse,scanl,scanl1,scanr,scanr1,span,splitAt,tail,take
  , takeWhile,unzip,unzip3,zip,zip3,zipWith,zipWith3,lines
  , unlines,unwords,words )
import Data.Function hiding ((.),($),const,flip,id)
import Data.Ord (comparing)

--import Prelude.Plus hiding (Sum, dup)
import Control.Category
import Cgm.Control.Combinators
import Cgm.Control.InFunctor
import Cgm.Data.Tagged
import Cgm.Data.Bool

{-# INLINE counting #-}
counting :: (Num a, Enum a) => a -> [a]
counting a = [0 .. pred a]