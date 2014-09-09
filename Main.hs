{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts,
             RecordWildCards #-}

module Main where

import Control.Comonad
import Control.Monad (forM, forM_, void)

import Data.Time

import Graphics.UI.WX hiding (Event, space)

import System.Exit

import Conway
import ListZipper
import Plane
import Torus
import Universe (Universe, Matrix(..))
import qualified Universe

data Creation = forall a. (Comonad a, Matrix a Bool, Universe a Bool) =>
                Creation (a Bool)

evolve' (Creation u) = Creation (evolve u)
up' (Creation u) = Creation (Universe.up u)
down' (Creation u) = Creation (Universe.down u)
left' (Creation u) = Creation (Universe.left u)
right' (Creation u) = Creation (Universe.right u)

god :: Int -> Int -> (Creation -> Creation) -> Creation -> Creation
god 0 0 backtrack (Creation u) =
  backtrack $
  if Universe.get u
  then
    Creation $ Universe.set False u
  else
    Creation $ Universe.set True u
god 0 y b c = god 0 (y-1) (b . up') (down' c)
god x y b c = god (x-1) y (b . left') (right' c)

ankh :: Int -> Int -> (Creation -> Creation) -> Creation -> Creation
ankh 0 0 backtrack (Creation u) =
  backtrack $ Creation $ Universe.set True u

ankh 0 y b c = ankh 0 (y-1) (b . up') (down' c)
ankh x y b c = ankh (x-1) y (b . left') (right' c)

maxXDim = 200
maxYDim = 200
minXDim = 3
minYDim = 3
maxSpeed = 2

data State =
  State
  { stateUniverse  :: Creation
  , stateBigBang   :: Creation
  , stateFrame     :: Frame ()
  , statePanel     :: Panel ()
  , stateTimer     :: Timer
  , stateRunning   :: Bool
  , stateSpeed     :: Int
  , stateXDim      :: Int
  , stateYDim      :: Int
  , stateLastFrame :: UTCTime }

main = do
  let universe = space 300 300 :: Torus Bool

  start $ do
    f  <- frame [ text := "Game of life"
                , outerSize := Size 500 500
                , closeable := True
                , on closing := exitSuccess ]
    p <- panel f [ bgcolor := black ]

    t <- timer f [ interval := 100 ]

    s <- getCurrentTime

    var <- varCreate $
           State
           (Creation universe)
           (Creation universe)
           f p t True 100 50 50 s

    set t [ on command  := evolveUniverse var ]
    set p [ on paint    := paintGrid var
          , on mouse    := clickGrid var
          , on keyboard := keyboardGrid var ]

paintGrid :: Var State -> DC b -> Rect -> IO ()
paintGrid var dc area = do
  State {..} <- varGet var
  now <- getCurrentTime
  let diff = diffUTCTime now stateLastFrame
  varUpdate var (\s -> s { stateLastFrame = now })
  drawText dc (show diff) (Point 0 0) [ textColor := white ]
  Creation universe <- return stateUniverse
  let grid = toMatrix universe
      xDim = min stateXDim (head . fmap length $ grid)
      yDim = min stateYDim (length grid)
      height = rectHeight area `quot` yDim
      width = rectWidth area `quot` xDim
  pVar <- varCreate (Rect 0 0 width height)
  forM_ (take yDim . iterate Universe.down $ universe) $ \universe' -> do
    forM_ (take xDim . iterate Universe.right $ universe') $ \universe'' -> do
      let state = extract universe''
      pos <- varGet pVar
      when state $ do
        drawRect dc pos [ bgcolor := white ]
      varUpdate pVar (\p -> p { rectLeft = rectLeft p + width })
    varUpdate pVar (\p -> p { rectTop = rectTop p + height,
                              rectLeft = 0 })

keyboardGrid :: Var State -> EventKey -> IO ()
keyboardGrid var (EventKey key Modifiers {..} _) = do
  case key of
    KeySpace -> do
      State {..} <- varGet var
      if stateRunning
        then do
        set stateTimer [ enabled := False ]
        varUpdate var (\s@(State {..}) -> s { stateRunning = False })
        return ()
        else do
        set stateTimer [ enabled := True ]
        varUpdate var (\s@(State {..}) -> s { stateRunning = True })
        return ()
    KeyUp -> do
      varUpdate var (\s@(State {..}) -> s { stateUniverse = down' stateUniverse })
      State {..} <- varGet var
      repaint statePanel
      return ()
    KeyDown -> do
      varUpdate var (\s@(State {..}) -> s { stateUniverse = up' stateUniverse })
      State {..} <- varGet var
      repaint statePanel
      return ()
    KeyLeft -> do
      varUpdate var (\s@(State {..}) -> s { stateUniverse = right' stateUniverse })
      State {..} <- varGet var
      repaint statePanel
      return ()
    KeyRight -> do
      varUpdate var (\s@(State {..}) -> s { stateUniverse = left' stateUniverse })
      State {..} <- varGet var
      repaint statePanel
      return ()
    KeyChar 'z' -> do
      State {..} <- varGet var
      if altDown
        then do
        let newXDim = min maxXDim $ stateXDim + 2
            newYDim = min maxYDim $ stateYDim + 2
        when (newXDim /= stateXDim && newYDim /= stateYDim) $ do
          varUpdate var (\s@(State {..}) ->
                          s { stateUniverse = up' . left' $ stateUniverse
                            , stateXDim = newXDim
                            , stateYDim = newYDim })
          repaint statePanel
          return ()
        else do
        let newXDim = max minXDim $ stateXDim - 2
            newYDim = max minYDim $ stateYDim - 2
        when (newXDim /= stateXDim && newYDim /= stateYDim) $ do
          varUpdate var (\s@(State {..}) ->
                          s { stateUniverse = down' . right' $ stateUniverse
                            , stateXDim = newXDim
                            , stateYDim = newYDim })
          repaint statePanel
          return ()
    KeyChar '+' -> do
      State {..} <- varGet var
      let newSpeed = max maxSpeed $ stateSpeed - 10
      set stateTimer [ interval := newSpeed ]
      varUpdate var (\s@(State {..}) -> s { stateSpeed = newSpeed })
      return ()
    KeyChar '-' -> do
      State {..} <- varGet var
      let newSpeed = stateSpeed + 10
      set stateTimer [ interval := newSpeed ]
      varUpdate var (\s@(State {..}) -> s { stateSpeed = newSpeed })
      return ()
    KeyChar 'c' -> do
      State {..} <- varGet var
      varUpdate var (\s@(State {..}) -> s { stateUniverse = stateBigBang })
      repaint statePanel
    _ -> return ()

clickGrid :: Var State -> EventMouse -> IO ()
clickGrid var event = do
  case event of
    MouseLeftDown Point {..} _ -> do
      State {..} <- varGet var
      Creation universe <- return stateUniverse
      area <- get statePanel outerSize
      let grid = toMatrix universe
          xDim = min stateXDim (head . fmap length $ grid)
          yDim = min stateYDim (length grid)
          height = sizeH area `quot` yDim
          width = sizeW area `quot` xDim
          x = max 0 $ pointX `quot` width
          y = max 0 $ pointY `quot` height
      varUpdate var (\s@(State {..}) ->
                      s { stateUniverse = god x y id stateUniverse })
      repaint statePanel
    MouseLeftDrag Point {..} _ -> do
      State {..} <- varGet var
      Creation universe <- return stateUniverse
      area <- get statePanel outerSize
      let grid = toMatrix universe
          xDim = min stateXDim (head . fmap length $ grid)
          yDim = min stateYDim (length grid)
          height = sizeH area `quot` yDim
          width = sizeW area `quot` xDim
          x = max 0 $ pointX `quot` width
          y = max 0 $ pointY `quot` height
      varUpdate var (\s@(State {..}) ->
                      s { stateUniverse = ankh x y id stateUniverse })
      repaint statePanel
    _ -> return ()

evolveUniverse :: Var State -> IO ()
evolveUniverse var = do
  State {..} <- varGet var
  varUpdate var (\s@(State {..}) ->
                  s { stateUniverse = evolve' stateUniverse })
  repaint statePanel
