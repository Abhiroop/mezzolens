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
module Mezzolens.State.Lazy
 ( zoom
 , use, uses
 , assign, update
 , (%=), (.=), (+=), (-=), (*=), (//=), (&&=), (||=), (<>=)
 , (%%=)
 , (<~)
 -- Reexports
 , Optical, Optical', SubStar, Constant, Compose, StateT
 , MonadState
 ) where


import Mezzolens
import Mezzolens.Profunctor

import Control.Monad.Trans.State.Lazy (StateT(..))
import Control.Monad.State.Lazy (MonadState, modify, state)
import qualified Control.Monad.State.Lazy as MSL
import Data.Functor.Compose (Compose(..))
import Data.Monoid ((<>))

zoom :: Optical' (SubStar (Compose m ((,) c))) ta a  -> StateT a m c -> StateT ta m c
-- ^ @
-- zoom :: Functor m => Lens' ta a -> StateT a m c -> StateT ta m c
-- zoom :: (Monoid c, Applicative m) => Traversal' ta a -> StateT a m c -> StateT ta m c
-- @
zoom l (StateT m) = StateT . zoomOut . l . zoomIn $ m
 where
  zoomIn f = Kleisli (Compose . f)
  zoomOut (Kleisli f) = getCompose . f

uses :: MonadState ta m => Optical (SubStar (Constant r)) ta tb a b -> (a -> r) -> m r
uses l f = MSL.gets (gets l f)

use :: MonadState ta m => Optical (SubStar (Constant a)) ta tb a b -> m a
use l = uses l id

infix 4 %=, .=, +=, -=, *=, //=, &&=, ||=, <>=
(%=) :: MonadState ta m => Optical (->) ta ta a b -> (a -> b) -> m ()
l %= f = modify (l f)
l .= x = l %= (const x)
l += x = l %= (+ x)
l -= x = l %= subtract x
l *= x = l %= (* x)
l //= x = l %= (/ x)
l &&= x = l %= (&& x)
l ||= x = l %= (|| x)
l <>= x = l %= (<> x)

assign :: MonadState ta m => Optical (->) ta ta a b -> b -> m ()
assign l x = l .= x

update :: MonadState ta m => Optical (->) ta ta a b -> (a -> b) -> m ()
update l f = l %= f

infixr 2 <~
(<~) :: MonadState ta m => Optical (->) ta ta a b -> m b -> m ()
l <~ x = assign l =<< x

infix 4 %%=
(%%=) :: MonadState ta m => Optical (SubStar ((,) c)) ta ta a b -> (a -> (c, b)) -> m c
-- ^ @
-- (%%=) :: MonadState ta m => Lens' ta a -> (a -> (c,a))  -> m c
-- (%%=) :: (Monoid c, MonadState ta m) => Traversal' ta a -> (a -> (c,a)) -> m c
-- @
l %%= f = state . runKleisli $ l (Kleisli f)

