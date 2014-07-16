{-# LANGUAGE TemplateHaskell #-}
module NCursesUI (newNCursesUI) where

import UI.NCurses
import Control.Lens
import System.IO
import Control.Monad.State
import Control.Concurrent

import UI
import UIUtils

type NCUI = StateT NCUIState IO

data View = MainMenu | GameView

data NCUIState = NCUIState {
    _ncUIState :: UIState,
    _ncView :: View
}

makeLenses ''NCUIState

simpleNCUIState :: NCUIState
simpleNCUIState = NCUIState {
    _ncUIState = simpleUIState,
    _ncView = MainMenu
}

newNCursesUI :: IO ()
newNCursesUI = void (runStateT start simpleNCUIState)

start :: NCUI ()
start = do
    liftIO $ runCurses $ do
        setEcho False
        w <- defaultWindow
        updateWindow w $ do
            moveCursor 1 10
            drawString "Hello world!"
            moveCursor 3 10
            drawString "(press q to quit)"
            moveCursor 0 0
            drawString $ show glyphGTE
        render
        waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop


    --until_ (use (ncUIState . uiQuit)) loop

loop :: NCUI ()
loop = do
    draw 
    zoomUI run
    liftIO wait

draw :: NCUI ()
draw = do
    view <- use ncView
    case view of
         MainMenu -> drawMainMenu
         otherwice -> return () 

drawMainMenu :: NCUI ()
drawMainMenu = return ()

wait :: IO ()
wait = threadDelay 40000 

zoomUI :: UI a -> NCUI a
zoomUI = zoom ncUIState
