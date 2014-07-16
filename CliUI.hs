{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}

module CliUI (newCliUI) where 

import Control.Applicative hiding ((<|>))
import Data.Maybe
import Control.Lens
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
    _cliUIState :: UIState,
    _cliInputBuffer :: String,
    _cliMessage :: String,
    _cliCommandHandlers :: M.Map String (CliUI ())
}

simpleCliUIState :: CliUIState
simpleCliUIState = CliUIState {
    _cliUIState = simpleUIState,
    _cliInputBuffer = "",
    _cliMessage = "",
    _cliCommandHandlers = M.empty
}

makeLenses ''CliUIState

newCliUI :: IO ()
newCliUI = void (runStateT start simpleCliUIState)

start :: CliUI ()
start = do
    uiHandlers
    liftIO $ hSetBuffering stdin NoBuffering
    liftIO $ hSetEcho stdin False
    until_ (use (cliUIState . uiQuit)) loop 

loop :: CliUI ()
loop = do
    draw
    handleInput
    zoomUI run
    wait

zoomUI :: UI a -> CliUI a
zoomUI = zoom cliUIState

draw :: CliUI ()
draw = do
    liftIO clearScreen
    liftIO $ setCursorPosition 0 0
    t <- use $ cliUIState . uiWorld . worldTerrain
    f <- use $ cliUIState . uiCamera . _3
    let floor = getFloor t f
    liftIO $ print floor
    message <- use cliMessage
    liftIO $ putStrLn message
    input <- use cliInputBuffer
    liftIO $ putStr ("> " ++ input)
    liftIO $ hFlush stdout

eval :: CliUI ()
eval = do
    str <- use cliInputBuffer
    cliInputBuffer .= ""
    let r = parseCommand str
    case r of
         Left _ -> return ()
         Right (cmd, arg) -> do
             zoomUI $ execCommand cmd arg
             handlers <- use cliCommandHandlers
             let h = M.lookup (cmd ^. commandName) handlers
             fromMaybe (return ()) h


handleKey :: Char -> CliUI ()
handleKey '\t' = do
  input <- use cliInputBuffer
  let choices = parseCompletion input
  when (length choices == 1) $ cliInputBuffer .= head choices 
handleKey '\DEL' = cliInputBuffer %= (\i -> take (length i - 1) i)
handleKey '\n' = eval
handleKey a 
    | a `elem` ['a'..'z']++['A'..'Z']
        ++['0'..'9']++"()," = cliInputBuffer %= (++ return a)
    | otherwise = return ()

handleInput :: CliUI ()
handleInput = do 
    r <- liftIO $ hReady stdin
    when r $ do
        c <- liftIO getChar 
        handleKey c
        handleInput

wait :: CliUI ()
wait = liftIO $ threadDelay 500000 

uiHandlers :: CliUI ()
uiHandlers = addHandler "help" (cliMessage .= unwords (commandNames allCommands))

addHandler :: String -> CliUI () -> CliUI ()
addHandler command handler = cliCommandHandlers %= M.insert command handler
