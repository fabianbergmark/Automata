module LoopZipper where

import Control.Comonad

import Data.Default
import Data.Monoid

data Loop s = Loop s [s]

loopRead (Loop s _) = s

loopWrite s (Loop _ r) = Loop s r

loopLength (Loop _ r) = 1 + length r

loopLeft (Loop s r) = Loop (last r) (s : init r)

loopRight (Loop s r) = Loop (head r) (tail r ++ [s])

toList :: Loop a -> [a]
toList (Loop s r) =
  s : r

instance (Default a) => Monoid (Loop a) where
  mempty = Loop def [def]
  mappend (Loop ls l) (Loop rs r) = Loop ls (l ++ (rs:r))

instance (Default a) => Default (Loop a) where
  def = mempty

instance Functor Loop where
  fmap f (Loop s r) = Loop (f s) (fmap f r)

instance Comonad Loop where
  extract = loopRead
  duplicate lz = Loop lz (take (pred .loopLength $ lz ) . tail . iterate loopRight $ lz)
