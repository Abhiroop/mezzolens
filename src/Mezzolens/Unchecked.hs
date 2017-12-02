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
module Mezzolens.Unchecked
 ( iso
 , lens, lensVL
 , prism
 , affineTraversal
 , traversal
 , sec
 -- Reexports
 , module Mezzolens.Optics
 , PStore
 ) where

import Prelude hiding (map)

import Mezzolens.Combinators
import Mezzolens.Profunctor
import Mezzolens.Optics

import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.Traversable (fmapDefault, foldMapDefault)

iso :: (ta -> a) -> (b -> tb) -> Iso ta tb a b
iso = dimap

lens :: (ta -> a) -> (b -> ta -> tb) -> Lens ta tb a b
lens get set = dimap (get &&& id) (uncurry set) . _1

--Foo p i j x needs to be covariant in i and x and contravariant in j.
--instance Strong p => Functor (Foo p i j)
--instance Wander p => Applicative (Foo p i j)
--Foo p i j x = p i j -> p () x
--idLike a = imap (const a)
--vl :: Strong p => ((a -> Foo p a b b) -> ta -> Foo p a b tb) -> Optical p ta tb a b
--vl l = dimap (l idlike) extractish . foo
-- where
--  extractish :: (Foo p b b tb) -> tb
--  foo :: p a b -> p (Foo p a c d) (Foo p b c d)

lensVL :: (forall f. Functor f => (a -> f b) -> ta -> f tb) -> Lens ta tb a b
lensVL l = dimap ((peek &&& pos) . l idPStore) (uncurry id) . _2

prism :: (ta -> Either tb a) -> (b -> tb) -> Prism ta tb a b
prism match beget = dimap match (id ||| beget) . _Right

-- sometimes known as a partial lens
affineTraversal :: (ta -> Either tb a) -> (b -> ta -> tb) -> AffineTraversal ta tb a b
affineTraversal match set = dimap f g . _Right . _1
 where
  f ta = (\x -> (x,ta)) <$> match ta
  g = id ||| uncurry set

traversal :: (forall f. Applicative f => (a -> f b) -> ta -> f tb) -> Traversal ta tb a b
traversal l = dimap f g . wander
 where
  f ta = TraversableFreeApplicativePStore (FreeApplicativePStore (flip l ta))
  g (TraversableFreeApplicativePStore (FreeApplicativePStore fps)) = runIdentity (fps Identity)

sec :: ((a -> b) -> ta -> tb) -> SEC ta tb a b
sec l = dimap (PCont . flip l) (($ id) . pcont) . map

data PStore i j x = PStore { peek :: j -> x, pos :: i }

instance Functor (PStore i j) where
  fmap f (PStore h i) = PStore (f . h) i

idPStore :: a -> PStore a b b
idPStore = PStore id

newtype PCont i j x = PCont { pcont :: (x -> j) -> i }

instance Functor (PCont i j) where
  fmap f (PCont k) = PCont $ k . (. f)

newtype FreeApplicativePStore i j x = FreeApplicativePStore { runFreeApplicativePStore :: forall f. Applicative f => (i -> f j) -> f x }

instance Functor (FreeApplicativePStore i j) where
  fmap f (FreeApplicativePStore fps) = FreeApplicativePStore $ (fmap f) . fps

instance Applicative (FreeApplicativePStore i j) where
  pure x = FreeApplicativePStore $ const (pure x)
  FreeApplicativePStore f <*> FreeApplicativePStore x = FreeApplicativePStore $ \op -> (f op) <*> (x op)

idFreeApplicativePStore :: a -> FreeApplicativePStore a b b
idFreeApplicativePStore a = FreeApplicativePStore ($ a)

newtype TraversableFreeApplicativePStore j x i = TraversableFreeApplicativePStore { getTraversableFreeApplicativePStore :: FreeApplicativePStore i j x }

instance Functor (TraversableFreeApplicativePStore j x) where
  fmap = fmapDefault

instance Foldable (TraversableFreeApplicativePStore j x) where
  foldMap = foldMapDefault

instance Traversable (TraversableFreeApplicativePStore j x) where
  traverse f (TraversableFreeApplicativePStore (FreeApplicativePStore fps)) = map TraversableFreeApplicativePStore . getCompose $
    fps (Compose . map idFreeApplicativePStore . f)
