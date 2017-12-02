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
module Mezzolens.Profunctor
  ( Profunctor(..), Strong(..), OutPhantom(..), Choice(..), InPhantom(..), Wandering(..), Cartographic(..)
  , _2Default, _RightDefault
  , ProProduct(..)
  , SuperStar(..), SubStar, Kleisli(..)
  ) where

import Prelude hiding (map)

import Mezzolens.Combinators
import Mezzolens.Phantom

import Control.Arrow (Kleisli(..))

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = imap f . omap g

  imap :: (a -> b) -> p b c -> p a c
  imap f = dimap f id

  omap :: (c -> d) -> p a c -> p a d
  omap g = dimap id g

instance Profunctor (->) where
  dimap f g h = g . h . f
  imap f h = h . f
  omap g h = g . h

class Profunctor p => Strong p where
  _1 :: p a b -> p (a, c) (b, c)
  _1 h = dimap swap swap (_2 h)

  _2 :: p a b -> p (c, a) (c, b)
  _2 h = dimap swap swap (_1 h)

instance Strong (->) where
  _2 = wander

class Strong p => OutPhantom p where
  ocoerce :: p c a -> p c b

_2Default :: OutPhantom p => p a b -> p (c, a) (c, b)
_2Default = ocoerce . imap snd

-- visit :: (Strong p, Functor t) => (forall f x. Functor f => t (f x) -> f (t x)) -> p a b -> p (t a) (t b)
visit :: (Strong p, Functor t) => (t (a, ()) -> (a, (t z))) -> p a b -> p (t a) (t b)
visit dist = dimap f g . _1
 where
  f = dist . fmap (\x -> (x,()))
  g (b, t1) = fmap (const b) t1

class Profunctor p => Choice p where
  _Left :: p a b -> p (Either a c) (Either b c)
  _Left h = dimap switch switch (_Right h)

  _Right :: p a b -> p (Either c a) (Either c b)
  _Right h = dimap switch switch (_Left h)

instance Choice (->) where
  _Right = wander

class Choice p => InPhantom p where
  icoerce :: p a c -> p b c

_RightDefault :: InPhantom p => p a b -> p (Either c a) (Either c b)
_RightDefault = icoerce . omap Right

-- It would be nice to come up with a better characterization of this class.
-- prod :: p a1 b1 -> p a2 b2 -> p (a1,a2) (b1,b2) could be one of the methods, but that doesn't appear to be sufficent.
class (Strong p, Choice p) => Wandering p where
  wander :: Traversable f => p a b -> p (f a) (f b)

instance Wandering (->) where
  wander = map

class Wandering p => Cartographic p where
  map :: Functor f => p a b -> p (f a) (f b)

instance Cartographic (->) where
  map = fmap

data ProProduct p q a b = ProProduct { upper :: p a b, lower :: q a b}
instance (Profunctor p, Profunctor q) => Profunctor (ProProduct p q) where
  dimap f g (ProProduct u l) = ProProduct (dimap f g u) (dimap f g l)

type SubStar = Kleisli

instance Functor f => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (fmap g . h . f)

instance Functor f => Strong (Kleisli f) where
  _2 (Kleisli h) = Kleisli $ \(x,y) -> (,) x <$> (h y)

instance Phantom f => OutPhantom (Kleisli f) where
  ocoerce (Kleisli h) = Kleisli $ coerce . h

instance Applicative f => Choice (Kleisli f) where
  _Right = wander

instance Applicative f => Wandering (Kleisli f) where
  wander (Kleisli h) = Kleisli (traverse h)

-- instance Identical f => Cartographic (Kleisli f) where
--   map (Kleisli h) = Kleisli $ pure . fmap (runIdentical . h)

newtype SuperStar f a b = SuperStar { runSuperStar :: f a -> b }

instance Functor f => Profunctor (SuperStar f) where
  dimap f g (SuperStar h) = SuperStar (g . h . fmap f)

instance Phantom f => Choice (SuperStar f) where
  _Left (SuperStar h) = SuperStar $ Left . h . coerce
  _Right (SuperStar h) = SuperStar $ Right . h . coerce

instance Phantom f => InPhantom (SuperStar f) where
  icoerce (SuperStar h) = SuperStar $ h . coerce

newtype ProIn p f a b = ProIn { proIn :: p (f a) b }

instance (Profunctor p, Functor f) => Profunctor (ProIn p f) where
  dimap f g (ProIn pab) = ProIn $ dimap (fmap f) g pab

instance (Profunctor p, Phantom f) => Choice (ProIn p f) where
  _Right = _RightDefault

instance (Profunctor p, Phantom f) => InPhantom (ProIn p f) where
  icoerce (ProIn pab) = ProIn $ imap coerce pab

newtype ProOut p g a b = ProOut { proOut :: p a (g b) }

instance (Profunctor p, Functor f) => Profunctor (ProOut p f) where
  dimap f g (ProOut pab) = ProOut $ dimap f (fmap g) pab

instance (Profunctor p, Phantom f) => Strong (ProOut p f) where
  _2 = _2Default

instance (Profunctor p, Phantom f) => OutPhantom (ProOut p f) where
  ocoerce (ProOut pab) = ProOut $ omap coerce pab
