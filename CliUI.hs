{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}

module CliUI (newCliUI) where 

import Control.Applicative hiding ((<|>))
import Control.Lens
import Control.Lens.Zoom
import System.Environment
import Control.Monad
import Control.Monad.State
import System.IO
import Control.Concurrent
import System.Console.ANSI
import Data.List
import qualified Data.Map as M

import UI
import UIUtils
import World
import Terrain

type CliUI = StateT CliUIState IO

data CliUIState = CliUIState {
    _cliUIState :: UIState
}

simpleCliUIState :: CliUIState
simpleCliUIState = CliUIState {
    _cliUIState = simpleUIState
}

makeLenses ''CliUIState

newCliUI :: IO ()
newCliUI = void (runStateT cliLoop simpleCliUIState)

cliLoop :: CliUI ()
cliLoop = do
    cliUIHandlers
    liftIO $ hSetBuffering stdin NoBuffering
    liftIO $ hSetEcho stdin False
    until_ (not <$> use (cliUIState . uiQuit)) (cliDraw >> handleInput >> zoomUI run >> wait)

zoomUI :: UI a -> CliUI a
zoomUI = zoom cliUIState

cliDraw :: CliUI ()
cliDraw = do
    liftIO clearScreen
    liftIO $ setCursorPosition 0 0
    t <- use $ cliUIState . uiWorld . worldTerrain
    f <- use $ cliUIState . uiCamera . _3
    let floor = getFloor t f
    liftIO $ print floor
    message <- use $ cliUIState . uiMessage
    liftIO $ putStrLn message
    input <- use $ cliUIState . uiInputBuffer
    liftIO $ putStr ("> " ++ input)
    liftIO $ hFlush stdout

cliEval :: CliUI ()
cliEval = do
    str <- use $ cliUIState . uiInputBuffer
    zoomUI $ uiInputBuffer .= ""
    zoomUI $ parseCommand str

until_ :: Monad m => m Bool -> m () -> m ()
until_ pred action = do
    action
    c <- pred
    when c $ until_ pred action 

handleKey :: Char -> CliUI ()
handleKey '\t' = do
  input <- use $ cliUIState . uiInputBuffer
  let choices = parseCompletion input
  when (length choices == 1) $ zoomUI $ uiInputBuffer .= head choices 
handleKey '\DEL' = zoomUI $ uiInputBuffer %= (\i -> take (length i - 1) i)
handleKey '\n' = cliEval
handleKey a 
    | a `elem` ['a'..'z']++['A'..'Z']
        ++['0'..'9']++"()," = zoomUI $ uiInputBuffer %= (++ return a)
    | otherwise = return ()

handleInput :: CliUI ()
handleInput = do 
    r <- liftIO $ hReady stdin
    when r $ do
        c <- liftIO getChar 
        handleKey c
        handleInput

wait :: CliUI ()
wait = liftIO $ threadDelay 500000 --ms

cliUIHandlers :: CliUI ()
cliUIHandlers = zoomUI $ addHandler "help" (uiMessage .= unwords (commandNames allCommands))
