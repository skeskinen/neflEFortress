{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AI where

import Control.Lens
import Control.Monad.State

import World
import Item
import Terrain

data Plan = 
    PlanPickUpItem ItemId
  | PlanMoveTo Point
  | PlanIdle

data AIAction = 
    AIPickUpItem ItemId
  | AIMove [Dir]
  | AIIdle 

data Dir = DUp | DLeft | DDown | DRight | DTop | DBottom

addDir :: Dir -> Point -> Point
addDir DUp     = _2 -~ 1
addDir DDown   = _2 +~ 1
addDir DLeft   = _1 -~ 1
addDir DRight  = _1 +~ 1
addDir DTop    = _3 -~ 1
addDir DBottom = _3 +~ 1

data AI = AI {
      _aiPlan       :: Plan
    , _aiActionList :: [AIAction]
}

defaultAI :: AI
defaultAI = AI {
      _aiPlan = PlanIdle
    , _aiActionList = []
}

makeLenses ''AI
makePrisms ''Plan
makePrisms ''AIAction

makeActions :: Creature AI -> State (World AI) AI
makeActions creature = case creature ^. creatureState . aiPlan of
    PlanIdle -> return $ creature ^. creatureState
                        & aiActionList .~ [ AIIdle
                                          , AIMove [DRight, DDown, DLeft, DUp]
                                          , AIIdle]
    _ -> return $ creature ^. creatureState 

runAI :: Creature AI -> State (World AI) (Creature AI)
runAI creature =
    case creature ^. creatureState . aiActionList of 
        [] -> do 
            actions <- makeActions creature 
            runAI (creature & creatureState .~ actions)
        (a:as) -> 
            case a of 
                AIMove (dir:dirs) -> do
                    let newpos = addDir dir $ creature ^. creaturePos
                    (newCreature, wasMoved) <- moveCreature creature newpos
                    return $ newCreature & creatureState . aiActionList .~ if wasMoved 
                       then AIMove dirs : as
                       else []
                _ -> return (creature & creatureState . aiActionList .~ as)

