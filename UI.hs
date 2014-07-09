{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Monad.State
import Control.Lens

import Utils
import Control.Lens
import World
import Terrain
import WorldGenerating

type UI = StateT UiState IO 

data UiState = UiState {
    _uiWorld :: World',
    _uiCamera :: Point,
    _uiQuit :: Bool
}

makeLenses ''UiState

startUi :: UI () -> IO ()
startUi ui = do
    runStateT ui simpleUiState
    return ()

simpleUiState :: UiState
simpleUiState = UiState {
      _uiWorld = simpleWorld,
      _uiCamera = (0,0,0),
      _uiQuit = False
}

data CommandArgument = NoTarget | PointArgument Point | AreaArgument Area
type CommandFunction = (CommandArgument -> UI ())

data Command = Command {
      _commandName :: String
    , _commandDescription :: String
    , _commandFunction :: CommandFunction
}

makeLenses ''Command

noTargetCommands :: [Command]
noTargetCommands = [quit, genWorld, advanceTurn, descendCamera, ascendCamera]

areaCommands :: [Command]
areaCommands = [dig]

pointCommands :: [Command]
pointCommands = [move]

cameraBounds :: UI ()
cameraBounds = return ()

quit :: Command 
quit = Command "quit" "" $ \_ ->
    uiQuit .= True
    
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

genWorld :: Command
genWorld = Command "genWorld" "" $ \_ -> do
    uiWorld .= simpleWorld

advanceTurn :: Command
advanceTurn = Command "advanceTurn" "" $ \_ -> do
    return () -- WTF?
    --uiWorld .= (execState (use uiWorld) stepWorld) --Nope
    --zoom uiWorld stepWorld --No ei ainakaan nain
