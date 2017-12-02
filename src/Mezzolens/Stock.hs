{-# LANGUAGE RankNTypes #-}
{-
Copyright 2015 Russell O'Connor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
module Mezzolens.Stock
 ( null
 , _curry, _flip, _swap, _switch
 , _1, _2
 , _Left, _Right
 , wander
 , map
 , eitherOne, eitherTwo, both
 , ix, fitting, binding, selecting
 , at, at', contains
 , intAt, intAt', intContains
 , _Just, _Nothing
 -- Reexports
 , module Mezzolens.Optics
 ) where

import Prelude hiding (map, null)

import Mezzolens.Optics
import Mezzolens.Combinators
import Mezzolens.Profunctor
import Mezzolens.Unchecked

import qualified Data.Map.Lazy as MapL
import qualified Data.Map.Strict as MapS
import qualified Data.Set as Set
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.IntSet as IntSet

null :: AffineTraversal ta ta a b
null = affineTraversal Left (flip const)

_curry :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
_curry = iso curry uncurry

_flip :: Iso (a -> b -> c) (d -> e -> f) (b -> a -> c) (e -> d -> f)
_flip = iso flip flip

_swap :: Iso (a,b) (c,d) (b,a) (d,c)
_swap = iso swap swap

_switch :: Iso (Either a b) (Either c d) (Either b a) (Either d c)
_switch = iso switch switch

eitherOne :: Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
eitherOne = iso (maybe (Left ()) Right) (const Nothing ||| Just)

eitherTwo :: Iso (Bool,a) (Bool,b) (Either a a) (Either b b)
eitherTwo = iso f ((,) False ||| (,) True)
 where
  f (False,a) = Left a
  f (True,a) = Right a

both :: Iso (Bool -> a) (Bool -> b) (a,a) (b,b)
both = iso to fro
 where
  to f = (f False, f True)
  fro p True = fst p
  fro p False = snd p

ix :: Eq k => k -> Lens' (k -> v) v
ix k = lens ($ k) (\v' g x -> if (k == x) then v' else g x)

fitting :: (k -> Bool) -> SEC' (k -> v) v
fitting p = sec $ \modify f k -> if p k then modify (f k) else f k

binding :: Eq k => k -> Prism' (k, v) v
binding i = prism (\kv@(k,v) -> if (i == k) then Right v else Left kv) ((,) i)

selecting :: (k -> Bool) -> AffineTraversal' (k, v) v
selecting p = affineTraversal (\kv@(k,v) -> if (p k) then Right v else Left kv) (\v' kv@(k,_) -> if (p k) then (k,v') else kv)

at :: Ord k => k -> Lens' (MapL.Map k v) (Maybe v)
at k = lens (MapL.lookup k) (maybe (MapL.delete k) (MapL.insert k))

at' :: Ord k => k -> Lens' (MapS.Map k v) (Maybe v)
at' k = lens (MapS.lookup k) (maybe (MapS.delete k) (MapS.insert k))

contains :: Ord k => k -> Lens' (Set.Set k) Bool
contains k = lens (Set.member k) (\nv -> if nv then Set.insert k else Set.delete k)

intAt :: Int -> Lens' (IntMapL.IntMap v) (Maybe v)
intAt k = lens (IntMapL.lookup k) (maybe (IntMapL.delete k) (IntMapL.insert k))

intAt' :: Int -> Lens' (IntMapS.IntMap v) (Maybe v)
intAt' k = lens (IntMapS.lookup k) (maybe (IntMapS.delete k) (IntMapS.insert k))

intContains :: Int -> Lens' IntSet.IntSet Bool
intContains k = lens (IntSet.member k) (\nv -> if nv then IntSet.insert k else IntSet.delete k)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = eitherOne._Right

_Nothing :: Prism' (Maybe a) ()
_Nothing = eitherOne._Left
