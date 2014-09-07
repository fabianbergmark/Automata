{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Comonad
import Control.Monad (forM, forM_, void)

import Graphics.UI.WX hiding (Event, space)

import Conway
import ListZipper
import Plane
import Torus
import Universe

paintGrid :: Matrix a Bool => Var (a Bool) -> DC b -> Rect -> IO ()
paintGrid var dc area = do
  universe <- varGet var
  let grid = toMatrix universe
      areaHeight = rectHeight area
      areaWidth = rectWidth area
      height = areaHeight `quot` length grid
      width = areaWidth `quot` (head . fmap length $ grid)
  pVar <- varCreate (Rect 0 0 width height)
  forM_ grid $ \row -> do
    forM_ row $ \state -> do
      pos <- varGet pVar
      let c = if state then white else black
      drawRect dc pos [bgcolor := c]
      varUpdate pVar (\p -> p { rectLeft = rectLeft p + width })
    varUpdate pVar (\p -> p { rectTop = rectTop p + height, rectLeft = 0 })

evolveUniverse :: (Comonad a, Matrix a Bool, Universe a Bool) =>
                  Var (a Bool) -> Panel () -> IO ()
evolveUniverse var p = do
  varUpdate var evolve
  repaint p

main = do
  let grid = (glider <| space 3 3 <| glider <| space 3 3 <| space 3 3) -^-
             space 15 1 -^-
             (space 3 3 <| glider <| space 3 3 <| glider <| space 3 3) -^-
             space 15 1 -^-
             (glider <| space 3 3 <| glider <| space 3 3 <| space 3 3) -^-
             space 15 1 -^-
             (space 3 3 <| glider <| space 3 3 <| glider <| space 3 3) -^-
             space 15 1

      universe = gliderGun <| space 10 11 -^- space 48 40

  start $ do
    var <- varCreate universe
    f  <- frame [ text := "Game of life"
                , outerSize := Size 500 500
                , resizeable := False ]
    p <- panel f [ on paint := paintGrid var ]
    t <- timer f [ interval := 100, on command := evolveUniverse var p ]
    return ()
