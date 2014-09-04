{-# LANGUAGE FlexibleContexts #-}

module Conway where

import Control.Comonad
import Control.Monad

import Data.Default

import ListZipper
import Plane
import Universe

instance Default Bool where
  def = False

neighbours :: Universe a b => [a b -> a b]
neighbours = [left, right, up, down,
              upleft, upright, downleft, downright]

aliveNeighbours :: (Universe a Bool, Comonad a) => a Bool -> Int
aliveNeighbours z =
  card $ map (\dir -> extract . dir $ z) neighbours

card :: [Bool] -> Int
card = length . filter id

conway :: (Universe a Bool, Comonad a) => a Bool -> Bool
conway z =
  case aliveNeighbours z of
    2 -> extract z
    3 -> True
    _ -> False

evolve :: (Universe a Bool, Comonad a) => a Bool -> a Bool
evolve = extend conway

glider :: Matrix a Bool => a Bool
glider = fromMatrix m
  where
    m = [ [f, t, f]
        , [f, f, t]
        , [t, t, t] ]
    t = True
    f = False

gliderGun :: Matrix a Bool => a Bool
gliderGun = fromMatrix m
  where
    m = [ [f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, t, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, t, f, t, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, f, t, t, f, f, f, f, f, f, t, t, f, f, f, f, f, f, f, f, f, f, f, f, t, t, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, t, f, f, f, t, f, f, f, f, t, t, f, f, f, f, f, f, f, f, f, f, f, f, t, t, f]
        , [f, t, t, f, f, f, f, f, f, f, f, t, f, f, f, f, f, t, f, f, f, t, t, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, t, t, f, f, f, f, f, f, f, f, t, f, f, f, t, f, t, t, f, f, f, f, t, f, t, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, t, f, f, f, f, f, t, f, f, f, f, f, f, f, t, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, t, f, f, f, t, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, f, t, t, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f] ]
    t = True
    f = False

space :: Matrix a Bool => Int -> Int -> a Bool
space x y = fromMatrix m
  where
    m = take y . repeat $ take x . repeat $ False
