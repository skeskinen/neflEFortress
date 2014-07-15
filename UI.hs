{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Monad.State
import Control.Monad
import Control.Lens
import qualified Data.Map as M

import Utils
import World
import Terrain
import WorldGenerating

type UI = StateT UIState IO 

data UIState = UIState {
    _uiWorld :: World',
    _uiCamera :: Point,
    _uiQuit :: Bool,
    _uiPause :: Bool
}

makeLenses ''UIState

simpleUIState :: UIState
simpleUIState = UIState {
      _uiWorld = simpleWorld,
      _uiCamera = (0,0,0),
      _uiQuit = False,
      _uiPause = False
}

data CommandArgument = NoTarget | PointArgument Point | AreaArgument Area | StringArgument String
    deriving Show
type CommandFunction = CommandArgument -> UI ()

data Command = Command {
      _commandName :: String
    , _commandDescription :: String
    , _commandFunction :: CommandFunction
}

makeLenses ''Command

run :: UI ()
run = do
    p <- use uiPause
    unless p $ uiWorld %= execState stepWorld

allCommands :: [Command]
allCommands = concat [noTargetCommands, areaCommands, pointCommands, stringCommands]

execCommand :: Command -> CommandArgument -> UI ()
execCommand cmd arg = (cmd ^. commandFunction) arg

noTargetCommands :: [Command]
noTargetCommands = 
  [
      Command "quit" "" (\_ -> uiQuit .= True)
    , Command "pause" "" (\_ -> uiPause %= not)
    , Command "descendCamera" "" (\_ -> (uiCamera . _3) += 1 >> cameraBounds)
    , Command "ascendCamera" "" (\_ -> (uiCamera . _3) += (-1) >> cameraBounds)
    , Command "genWorld" "" (\_ -> uiWorld .= simpleWorld)
    , Command "advanceTurn" "" (\_ -> uiWorld %= execState stepWorld)
  ]

areaCommands :: [Command]
areaCommands = 
  [
    Command "dig" "" $ \a -> return ()
  ]

pointCommands :: [Command]
pointCommands = 
  [
    Command "move" "" $ \p -> return ()
  ]

stringCommands :: [Command]
stringCommands = 
  [
    Command "help" "" $ \s -> return ()
  ]

cameraBounds :: UI ()
cameraBounds = return ()

