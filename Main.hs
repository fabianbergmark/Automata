{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Comonad
import Control.Monad (forM)

import Conway
import ListZipper
import Plane
import Torus
import Universe

disp :: Matrix a Bool => a Bool -> Int -> Int -> String
disp a x y =
  unlines . (\ls -> hr:ls ++ [hr]) . take y . map dispLine $ toMatrix a
  where
    hr = take x $ repeat '-'
    dispLine l = (++ "|") . take x $ map (\b -> if b then '*' else ' ') l

main = do
  let grid = (glider <| space 3 3 <| glider <| space 3 3 <| space 3 3) -^-
             space 15 1 -^-
             (space 3 3 <| glider <| space 3 3 <| glider <| space 3 3) -^-
             space 15 1 -^-
             (glider <| space 3 3 <| glider <| space 3 3 <| space 3 3) -^-
             space 15 1 -^-
             (space 3 3 <| glider <| space 3 3 <| glider <| space 3 3) -^-
             space 15 1

      universe = (universe1 -^- universe1) <|
                 (universe1 -^- universe1) <|
                 (universe1 -^- universe1)

  forM (iterate evolve universe) $ \game -> do
    putStrLn $ disp game 50 50
    getChar
