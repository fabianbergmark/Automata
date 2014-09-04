{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Torus where

import LoopZipper
import Universe

import Control.Comonad

import Data.Default
import Data.Monoid

data Torus a = Torus (Loop (Loop a))

(<|) :: Default a => Torus a -> Torus a -> Torus a
(<|) (Torus (Loop l ld)) (Torus (Loop r rd)) =
  Torus (Loop (l <> r) (zipWith (<>) ld rd))

(-^-) :: Default a => Torus a -> Torus a -> Torus a
(-^-) (Torus t) (Torus b) =
  Torus (t <> b)

torusShape (Torus a) = (loopLength a, extract $ fmap loopLength a)

instance Universe Torus a where
  left (Torus z) = Torus (fmap loopLeft z)
  right (Torus z) = Torus (fmap loopRight z)
  up (Torus z) = Torus (loopLeft z)
  down (Torus z) = Torus (loopRight z)

instance Matrix Torus a where
  fromMatrix m = Torus $ Loop (head w) (tail w)
    where
      w = (map (\(h:t) -> Loop h t) m)
  toMatrix (Torus (Loop m d)) = toRow m : fmap toRow d
    where
      toRow (Loop m r) = m:r

instance Functor Torus where
  fmap f (Torus z) = Torus (fmap (fmap f) z)

instance Comonad Torus where
  extract (Torus z) = loopRead . loopRead $ z
  duplicate u =
    Torus $ fmap
    (\r -> Loop r (take (pred . snd . torusShape $ r) . tail . iterate right $ r))
    (Loop u (take (pred . fst . torusShape $ u) . tail . iterate down $ u))
