module CliUI (newCliUI) where 

import Control.Applicative hiding ((<|>))
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

cliDraw :: UI ()
cliDraw = do
    liftIO clearScreen
    liftIO $ setCursorPosition 0 0
    t <- use (uiWorld . worldTerrain)
    f <- use (uiCamera . _3)
    let floor = getFloor t f
    liftIO $ print floor
    message <- use uiMessage
    liftIO $ putStrLn message
    input <- use uiInputBuffer
    liftIO $ putStr ("> " ++ input)
    liftIO $ hFlush stdout

cliEval :: UI ()
cliEval = do
    str <- use uiInputBuffer
    uiInputBuffer .= ""
    parseCommand str

until_ :: Monad m => m Bool -> m () -> m ()
until_ pred action = do
    action
    c <- pred
    when c $ until_ pred action 

handleKey :: Char -> UI ()
handleKey '\t' = do
  input <- use uiInputBuffer
  let choices = parseCompletion input
  when (length choices == 1) $ uiInputBuffer .= head choices 
handleKey '\DEL' = uiInputBuffer %= (\i -> take (length i - 1) i)
handleKey '\n' = cliEval
handleKey a 
    | a `elem` ['a'..'z']++['A'..'Z']
        ++['0'..'9']++"()," = uiInputBuffer %= (++ return a)
    | otherwise = return ()

handleInput :: UI ()
handleInput = do 
    r <- liftIO $ hReady stdin
    when r $ do
        c <- liftIO getChar 
        handleKey c
        handleInput

wait :: UI ()
wait = liftIO $ threadDelay 500000 --ms

newCliUI = do
    cliUIHandlers
    liftIO $ hSetBuffering stdin NoBuffering
    liftIO $ hSetEcho stdin False
    until_ (not <$> use uiQuit) (cliDraw >> handleInput >> run >> wait)

cliUIHandlers :: UI ()
cliUIHandlers = addHandler "help" (uiMessage .= unwords (commandNames allCommands))
