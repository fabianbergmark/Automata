{-# LANGUAGE BangPatterns #-}

module LoopZipper where

import Control.Comonad

import Data.Default
import qualified Data.Foldable as Foldable
import Data.Monoid
import Data.Sequence

import Prelude hiding (length, take)
import qualified Prelude

data Loop s = Loop !s !(Seq s)

loopRead (Loop s _) = s

loopWrite s (Loop _ r) = Loop s r

loopLength (Loop _ r) = 1 + length r

loopLeft (Loop s r) =
  let (!init) :> (!last) = viewr r in
  Loop last (s <| init)

loopRight (Loop s r) =
  let (!head) :< (!tail) = viewl r in
  Loop head (tail |> s)

toList :: Loop a -> [a]
toList (Loop s r) =
  s : (Foldable.toList r)

seqTail :: Seq a -> Seq a
seqTail !s =
  case viewl s of
    EmptyL -> s
    _ :< t -> t

instance (Default a) => Monoid (Loop a) where
  mempty = Loop def $ fromList [def]
  mappend (Loop ls l) (Loop rs r) = Loop ls (l >< (rs <| r))

instance (Default a) => Default (Loop a) where
  def = mempty

instance Functor Loop where
  fmap f (Loop s r) = Loop (f s) (fmap f r)

instance Comonad Loop where
  extract = loopRead
  duplicate !lz = Loop lz (seqTail . iterateN ((+ 1) . pred . loopLength $ lz) loopRight $ lz)
