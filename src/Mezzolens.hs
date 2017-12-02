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
module Mezzolens
  ( set, modifyF, match
  , get, gets, beget
  , toListOf, firstOf, sumOf, productOf, allOf, anyOf, lengthOf, nullOf
  , to, fro
  , un
  , alongside, eitherside
  , (^.), (^..), (^?)
  , (.~)
  -- Rexports
  , module Mezzolens.Optics
  , SuperStar, SubStar
  , Constant, First, Sum, Product, All, Any
  , AlongSide, EitherSide
  ) where

import Mezzolens.Combinators
import Mezzolens.Profunctor
import Mezzolens.Optics
import Mezzolens.Unchecked

import Data.Functor.Constant (Constant(..))
import Data.Monoid (All(..), Any(..), First(..), Product(..), Sum(..))

gets :: Optical (SubStar (Constant r)) ta tb a b -> (a -> r) -> ta -> r
-- ^ @
-- gets :: To ta tb a b -> (a -> r) -> ta -> r
-- gets :: Monoid r => Fold ta tb a b -> (a -> r) -> ta -> r
-- @
gets l f = getConstant . h
 where
  Kleisli h = l (Kleisli (Constant . f))

get :: Optical (SubStar (Constant a)) ta tb a b -> ta -> a
-- ^ @
-- get :: To ta tb a b -> ta -> a
-- get :: Monoid a => Fold ta tb a b -> ta -> a
-- @
get l = gets l id

beget :: Optical (SuperStar (Constant b)) ta tb a b -> b -> tb
-- ^ @
-- beget :: Fro ta tb a b -> b -> tb
-- @
beget l = h . Constant
 where
  SuperStar h = l (SuperStar getConstant)

set :: ((a -> b) -> c) -> b -> c
-- ^ @
-- set :: SEC ta tb a b -> b -> ta -> tb
-- @
set l = l . const

modifyF :: Optical (SubStar f) ta tb a b -> (a -> f b) -> ta -> f tb
-- ^ @
-- modifyF :: Functor f => Lens ta tb a b -> (a -> f b) -> ta -> f tb
-- modifyF :: Applicative f => Traversal ta tb a b -> (a -> f b) -> ta -> f tb
-- @
modifyF l f = tf
 where
  Kleisli tf = l (Kleisli f)

match :: Optical (SubStar (Either a)) ta tb a b -> ta -> Either tb a
-- ^ @
-- match :: Traversal ta tb a b -> ta -> Either tb a
-- @
match l = switch . h
 where
  Kleisli h = l (Kleisli Left)

toListOf :: Applicative f => Optical (SubStar (Constant (f a))) ta tb a b -> ta -> f a
-- ^ @
-- toListOf :: Fold ta tb a b -> ta -> [a]
-- toListOf :: (Applicative f, Monoid (f a)) => Fold ta tb a b -> ta -> f a
-- toListOf :: Applicative f => To ta tb a b -> ta -> f a
-- @
toListOf l = gets l pure

firstOf :: Optical (SubStar (Constant (First a))) ta tb a b -> ta -> Maybe a
-- ^ @
-- firstOf :: Fold ta tb a b -> ta -> Maybe a
-- @
firstOf l = getFirst . gets l (First . pure)

sumOf :: Optical (SubStar (Constant (Sum a))) ta tb a b -> ta -> a
sumOf l = getSum . gets l Sum

productOf :: Optical (SubStar (Constant (Product a))) ta tb a b -> ta -> a
productOf l = getProduct . gets l Product

allOf :: Optical (SubStar (Constant All)) ta tb a b -> (a -> Bool) -> ta -> Bool
allOf l p = getAll . gets l (All . p)

anyOf :: Optical (SubStar (Constant Any)) ta tb a b -> (a -> Bool) -> ta -> Bool
anyOf l p = getAny . gets l (Any . p)

lengthOf :: Num r => Optical (SubStar (Constant (Sum r))) ta tb a b -> ta -> r
lengthOf l = getSum . gets l (const (Sum 1))

nullOf :: Optical (SubStar (Constant All)) ta tb a b -> ta -> Bool
nullOf l = allOf l (const False)

infixl 8 ^., ^.., ^?
infixr 4 .~

x^.l = get l x
x^..l = toListOf l x
x^?l = firstOf l x
l.~x = set l x

to :: (ta -> a) -> To ta tb a b
to f = ocoerce . imap f

fro :: (b -> tb) -> Fro ta tb a b
fro f = icoerce . omap f

un :: Optical (ProProduct (SubStar (Constant tb)) (SuperStar (Constant ta))) b a tb ta -> Iso ta tb a b
-- ^ @
-- un :: Iso b a tb ta -> Iso ta tb a b
-- @
un l = iso (beget . Constant) (getConstant . get)
 where
  ProProduct (Kleisli get) (SuperStar beget) = l (ProProduct (Kleisli Constant) (SuperStar getConstant))

alongside :: Profunctor p => Optical (AlongSide p sc sd) ta tb a b -> Optical (AlongSide p a b) sc sd c d -> Optical p (ta,sc) (tb,sd) (a,c) (b,d)
-- ^ @
-- alongside :: Iso ta tb a b -> Iso sc sd c d -> Iso (ta,sc) (tb,sd) (a,c) (b,d)
-- alongside :: Lens ta tb a b -> Lens sc sd c d -> Lens (ta,sc) (tb,sd) (a,c) (b,d)
-- alongside :: To ta tb a b -> To sc sd c d -> To (ta,sc) (tb,sd) (a,c) (b,d)
-- @
alongside lab lcd = dimap swap swap . runAlongSide . lab . AlongSide . dimap swap swap . runAlongSide . lcd . AlongSide

eitherside :: Profunctor p => Optical (EitherSide p sc sd) ta tb a b -> Optical (EitherSide p a b) sc sd c d -> Optical p (Either ta sc) (Either tb sd) (Either a c) (Either b d)
-- ^ @
-- eitherside :: Iso ta tb a b -> Iso sc sd c d -> Iso (Either ta sc) (Either tb sd) (Either a c) (Either b d)
-- eitherside :: Prism ta tb a b -> Prism sc sd c d -> Lens (Either ta sc) (Either tb sd) (Either a c) (Either b d)
-- eitherside :: Fro ta tb a b -> Fro sc sd c d -> To (Either ta sc) (Either tb sd) (Either a c) (Either b d)
-- @
eitherside lab lcd = dimap switch switch . runEitherSide . lab . EitherSide . dimap switch switch . runEitherSide . lcd . EitherSide

newtype AlongSide p c d a b = AlongSide { runAlongSide :: p (c,a) (d,b) }

instance Profunctor p => Profunctor (AlongSide p c d) where
  dimap f g (AlongSide pab) = AlongSide $ dimap (fmap f) (fmap g) pab

instance Strong p => Strong (AlongSide p c d) where
  _2 (AlongSide pab) = AlongSide . dimap shuffle shuffle . _2 $ pab
   where
    shuffle (x,(y,z)) = (y,(x,z))

instance OutPhantom p => OutPhantom (AlongSide p c d) where
  ocoerce (AlongSide pab) = AlongSide $ ocoerce pab

newtype EitherSide p c d a b = EitherSide { runEitherSide :: p (Either c a) (Either d b) }

instance Profunctor p => Profunctor (EitherSide p c d) where
  dimap f g (EitherSide pab) = EitherSide $ dimap (fmap f) (fmap g) pab

instance Choice p => Choice (EitherSide p c d) where
  _Right (EitherSide pab) = EitherSide . dimap shuffle shuffle . _Right $ pab
   where
    shuffle = Right . Left ||| (Left ||| Right . Right)

instance InPhantom p => InPhantom (EitherSide p c d) where
  icoerce (EitherSide pab) = EitherSide $ icoerce pab

{-
choosing :: Strong p => Optical (ProEither p a b) ta tb a b -> Optical (ProEither p ta tb) sa sb a b -> Optical p (Either ta sa) (Either tb sb) a b
choosing l1 l2 = profg . l2 . Profg . dimap switch switch . profg . l1 . Profg . dimap f g . _2
 where
  f = (,) False ||| (,) True
  g (False,a) = Left a
  g (True,b) = Right b

newtype Profg p f g a b = Profg { profg :: p (f a) (g b) }

instance (Profunctor p, Functor f, Functor g) => Profunctor (Profg p f g) where
  dimap f g (Profg pab) = Profg $ dimap (fmap f) (fmap g) pab

type ProEither p x y = Profg p (Either x) (Either y)
-}
