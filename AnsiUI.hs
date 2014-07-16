{-# LANGUAGE TemplateHaskell #-}
module AnsiUI (newAnsiUI) where

import System.Console.ANSI
import System.Console.Terminal.Size
import Control.Lens
import System.IO
import Control.Monad.State
import Control.Concurrent

import UI
import UIUtils

type AnsiUI = StateT AnsiUIState IO

data View = MainMenu | GameView

data AnsiUIState = AnsiUIState {
    _ansiUIState :: UIState,
    _ansiView :: View
}

makeLenses ''AnsiUIState

simpleAnsiUIState :: AnsiUIState
simpleAnsiUIState = AnsiUIState {
    _ansiUIState = simpleUIState,
    _ansiView = MainMenu
}

newAnsiUI :: IO ()
--newAnsiUI = void (runStateT start simpleAnsiUIState)
newAnsiUI = print size

start :: AnsiUI ()
start = do
    liftIO $ hSetBuffering stdin NoBuffering
    liftIO $ hSetEcho stdin False
    until_ (use (ansiUIState . uiQuit)) loop

loop :: AnsiUI ()
loop = do
    draw 
    zoomUI run
    liftIO wait

draw :: AnsiUI ()
draw = do
    view <- use ansiView
    case view of
         MainMenu -> drawMainMenu
         otherwice -> return () 

drawMainMenu :: AnsiUI ()
drawMainMenu = return ()

wait :: IO ()
wait = threadDelay 40000 

zoomUI :: UI a -> AnsiUI a
zoomUI = zoom ansiUIState
