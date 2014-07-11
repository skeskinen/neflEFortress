{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Monad.State
import Control.Lens

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
    _uiFrameCounter :: Int
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
      _uiFrameCounter = 0
}

data CommandArgument = NoTarget | PointArgument Point | AreaArgument Area
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

noTargetCommands :: [Command]
noTargetCommands = [pause, quit, genWorld, advanceTurn, descendCamera, ascendCamera]

areaCommands :: [Command]
areaCommands = [dig]

pointCommands :: [Command]
pointCommands = [move]

cameraBounds :: UI ()
cameraBounds = return ()

quit :: Command 
quit = Command "quit" "" $ \_ ->
    uiQuit .= True

pause :: Command
pause = Command "pause" "" $ \_ ->
    uiPause %= not
    
descendCamera :: Command 
descendCamera = Command "descendCamera" "" $ \_ ->
    (uiCamera . _3) += 1 >> cameraBounds

ascendCamera :: Command
ascendCamera = Command "ascendCamera" "" $ \_ ->
    (uiCamera . _3) += (-1) >> cameraBounds

dig :: Command 
dig = Command "dig" "" $ \a -> do
    return ()

move :: Command
move = Command "move" "" $ \p -> do
    return ()

genWorld = Command "genWorld" "" $ \_ -> do
    uiWorld .= simpleWorld

advanceTurn :: Command
advanceTurn = Command "advanceTurn" "" $ \_ -> do
    uiWorld %= execState stepWorld
