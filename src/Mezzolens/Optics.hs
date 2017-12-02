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
module Mezzolens.Optics
 ( Optical
 , Iso, Lens, To, Prism, Fro, AffineTraversal, Traversal, Fold, SEC
 , Optical'
 , Simple
 , Iso', Lens', Prism', AffineTraversal', Traversal', SEC'
 -- Rexports
 , Profunctor, Strong, OutPhantom, Choice, InPhantom, Wandering, Cartographic
 ) where

import Mezzolens.Profunctor

type Optical p ta tb a b = p a b -> p ta tb

type Iso ta tb a b = forall p. Profunctor p => Optical p ta tb a b
type Lens ta tb a b = forall p. Strong p => Optical p ta tb a b
type To ta tb a b = forall p. OutPhantom p => Optical p ta tb a b
type Prism ta tb a b = forall p. Choice p => Optical p ta tb a b
type Fro ta tb a b = forall p. InPhantom p => Optical p ta tb a b
type AffineTraversal ta tb a b = forall p. (Strong p, Choice p) => Optical p ta tb a b
type Traversal ta tb a b = forall p. Wandering p => Optical p ta tb a b
type Fold ta tb a b = forall p. (OutPhantom p, Wandering p) => Optical p ta tb a b
type SEC ta tb a b = forall p. Cartographic p => Optical p ta tb a b

type Optical' p ta a = p a a -> p ta ta

type Simple f ta a = f ta ta a a
type Iso' ta a = Simple Iso ta a
type Lens' ta a = Simple Lens ta a
type Prism' ta a = Simple Prism ta a
type AffineTraversal' ta a = Simple AffineTraversal ta a
type Traversal' ta a = Simple Traversal ta a
type SEC' ta a = Simple SEC ta a
