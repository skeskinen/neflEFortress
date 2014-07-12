{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

import Utils
import Control.Lens
import World
import Terrain
import WorldGenerating

type UI = StateT UIState IO 

data UIState = UIState {
    _uiWorld :: World',
    _uiCamera :: Point,
    _uiQuit :: Bool,
    _uiPause :: Bool,
    _uiInputBuffer :: String,
    _uiFrameCounter :: Int,
    _uiCommandHandlers :: M.Map String (UI ()),
    _uiMessage :: String
}

makeLenses ''UIState

startUI :: UI () -> IO ()
startUI ui = do
    runStateT ui simpleUIState
    return ()

simpleUIState :: UIState
simpleUIState = UIState {
      _uiWorld = simpleWorld,
      _uiCamera = (0,0,0),
      _uiQuit = False,
      _uiPause = False,
      _uiInputBuffer = "",
      _uiFrameCounter = 0,
      _uiCommandHandlers = M.empty,
      _uiMessage = ""
}

data CommandArgument = NoTarget | PointArgument Point | AreaArgument Area | StringArgument String
    deriving Show
type CommandFunction = (CommandArgument -> UI ())

data Command = Command {
      _commandName :: String
    , _commandDescription :: String
    , _commandFunction :: CommandFunction
}

makeLenses ''Command

run :: UI ()
run = do
    p <- use uiPause
    if (not p) then uiWorld %= execState stepWorld else return ()

allCommands :: [Command]
allCommands = concat [noTargetCommands, areaCommands, pointCommands, stringCommands]

execCommand :: Command -> CommandArgument -> UI ()
execCommand cmd arg = do
    (cmd ^. commandFunction) arg
    handlers <- use uiCommandHandlers
    let h = M.lookup (cmd ^. commandName) handlers
    case h of
         Just handler -> handler
         Nothing -> return ()

noTargetCommands :: [Command]
noTargetCommands = 
  [
      Command "quit" "" (\_ -> do
        uiQuit .= True)
    , Command "pause" "" (\_ -> do
        uiPause %= not)
    , Command "descendCamera" "" (\_ -> do
        (uiCamera . _3) += 1 >> cameraBounds)
    , Command "ascendCamera" "" (\_ -> do
        (uiCamera . _3) += (-1) >> cameraBounds)
    , Command "genWorld" "" (\_ -> do
        uiWorld .= simpleWorld)
    , Command "advanceTurn" "" (\_ -> do
        uiWorld %= execState stepWorld)
  ]

areaCommands :: [Command]
areaCommands = 
  [
    Command "dig" "" $ \a -> do
        return ()
  ]

pointCommands :: [Command]
pointCommands = 
  [
    Command "move" "" $ \p -> do
        return ()
  ]

stringCommands :: [Command]
stringCommands = 
  [
    Command "help" "" $ \s -> do
        return ()
  ]

cameraBounds :: UI ()
cameraBounds = return ()

