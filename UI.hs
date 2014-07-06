{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Lens
import World
import Terrain
import WorldGenerating

import Control.Monad.State
import Control.Lens

type UI = StateT UiState IO 

data UiState = UiState {
    _uiWorld :: World
}

makeLenses ''UiState

startUi :: UI () -> IO ()
startUi ui = do
    runStateT ui simpleUiState
    return ()

simpleUiState :: UiState
simpleUiState = UiState {
      _uiWorld = simpleWorld
}

data CommandArgument = NoTarget | PointArgument Point | AreaArgument Area
type CommandFunction = (CommandArgument -> UI ())

data Command = Command {
      _commandName :: String
    , _commandDescription :: String
    , _commandFunction :: CommandFunction
}

makeLenses ''Command

type NoTarget = UI ()
type Targeted a = a -> UI ()
type AreaTarget = Targeted Area
type PointTarget = Targeted Point

noTargetCommands :: [Command]
noTargetCommands = [genWorld, advanceTurn]

areaCommands :: [Command]
areaCommands = [dig]

pointCommands :: [Command]
pointCommands = [move]

createCommand :: String -> CommandFunction -> Command 
createCommand name f = Command name "" f

dig :: Command 
dig = createCommand "dig" $ \a -> do
    return ()

move :: Command
move = createCommand "move" $ \p -> do
    return ()

genWorld :: Command
genWorld = createCommand "genWorld" $ \_ -> do
    return ()

advanceTurn :: Command
advanceTurn = createCommand "advanceTurn" $ \_ -> do
    return ()
