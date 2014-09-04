module ListZipper where

import Control.Comonad

data Zipper s = Zipper [s] s [s]

lzRead (Zipper _ s _) = s

lzWrite s (Zipper l _ r) = Zipper l s r

lzLeft (Zipper (l:ls) c r) = Zipper ls l (c:r)

lzRight (Zipper l c (r:rs)) = Zipper (c:l) r rs

toList :: Zipper a -> Int -> [a]
toList (Zipper ls x rs) n =
  reverse (take n ls) ++ [x] ++ take n rs

instance Functor Zipper where
  fmap f (Zipper l c r) = Zipper (fmap f l) (f c) (fmap f r)

instance Comonad Zipper where
  extract (Zipper _ c _) = c
  duplicate lz = Zipper (tail $ iterate lzLeft lz) lz (tail $ iterate lzRight lz)
