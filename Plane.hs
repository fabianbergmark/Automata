{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Plane where

import ListZipper
import Universe

import Control.Comonad

import Data.Default

data Plane z = Plane (Zipper (Zipper z))

instance Universe Plane a where
  left (Plane z) = Plane (fmap lzLeft z)
  right (Plane z) = Plane (fmap lzRight z)
  up (Plane z) = Plane (lzLeft z)
  down (Plane z) = Plane (lzRight z)

instance (Default a) => Matrix Plane a where
  fromMatrix m = Plane $ Zipper rzf (head w) (tail w ++ rzf)
    where
      w = (map (\l -> Zipper rf f (l ++ rf)) m)
      f = def
      rf = repeat f
      zf = Zipper rf f rf
      rzf = repeat zf
  toMatrix (Plane (Zipper _ m d)) = toRow m : fmap toRow d
    where
      toRow (Zipper _ m r) = m:r

instance Functor Plane where
  fmap f (Plane z) = Plane (fmap (fmap f) z)

instance Comonad Plane where
  extract (Plane z) = lzRead . lzRead $ z
  duplicate u =
    Plane $ fmap
    (\r -> Zipper (tail $ iterate left r) r (tail $ iterate right r))
    (Zipper (tail $ iterate up u) u (tail $ iterate down u))
