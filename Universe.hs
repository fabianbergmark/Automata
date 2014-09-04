{-# LANGUAGE MultiParamTypeClasses #-}

module Universe where

import Control.Comonad

import Data.Default

class Universe a b where
  left       :: a b -> a b
  right      :: a b -> a b
  up         :: a b -> a b
  down       :: a b -> a b
  upleft     :: a b -> a b
  upright    :: a b -> a b
  downleft   :: a b -> a b
  downright  :: a b -> a b
  upleft     = left . up
  upright    = right . up
  downleft   = left . down
  downright  = right . down

class Matrix a b where
  fromMatrix :: [[b]] -> a b
  toMatrix   :: a b -> [[b]]
