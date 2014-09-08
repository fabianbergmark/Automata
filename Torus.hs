{-# LANGUAGE BangPatterns,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module Torus where

import LoopZipper
import Universe

import Control.Comonad

import Data.Default
import qualified Data.Foldable as Foldable
import Data.Monoid
import Data.Sequence

import Prelude hiding (take, zipWith)
import qualified Prelude

data Torus a = Torus !(Loop (Loop a))

(<|) :: Default a => Torus a -> Torus a -> Torus a
(<|) (Torus (Loop l ld)) (Torus (Loop r rd)) =
  Torus (Loop (l <> r) (zipWith (<>) ld rd))

(-^-) :: Default a => Torus a -> Torus a -> Torus a
(-^-) (Torus t) (Torus b) =
  Torus (t <> b)

torusShape (Torus a) = (loopLength a, extract $ fmap loopLength a)

instance Universe Torus a where
  get = extract
  set s (Torus (Loop (Loop _ rs) ds)) = Torus (Loop (Loop s rs) ds)
  left (Torus z) = Torus (fmap loopLeft z)
  right (Torus z) = Torus (fmap loopRight z)
  up (Torus z) = Torus (loopLeft z)
  down (Torus z) = Torus (loopRight z)

instance Matrix Torus a where
  fromMatrix m = Torus $ Loop (head w) (fromList . tail $ w)
    where
      w = map (\(h:t) -> Loop h (fromList t)) m
  toMatrix (Torus (Loop m d)) = toRow m : (Foldable.toList $ fmap toRow d)
    where
      toRow (Loop m r) = m : (Foldable.toList r)

instance Functor Torus where
  fmap f (Torus z) = Torus (fmap (fmap f) z)

instance Comonad Torus where
  extract (Torus z) = loopRead . loopRead $ z
  duplicate !u =
    Torus $ fmap
    (\ !r -> Loop r (seqTail . iterateN ((+ 1) . pred . snd . torusShape $ r) right $ r))
    (Loop u (seqTail . iterateN ((+ 1) . pred . fst . torusShape $ u) down $ u))
