{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AIState where

import Control.Lens

import Building
import Item
import PathFinding
import Terrain
import Utils

data Plan world c = 
    PlanPickUpItem (ItemIdP world c)
  | PlanMoveTo Point
  | PlanDig Point
  | PlanBuild (BuildingIdP world)
  | PlanIdle
  | PlanOther
  deriving Show

data PlanState =
    PlanStarted
  | PlanFinished
  deriving Show

data AIAction world c = 
    AIPickUpItem (ItemIdP world c)
  | AIMove [Dir]
  | AIDig Dir
  | AIBuild
  | AIIdle 
  deriving Show

data AIState world c = AI {
      _aiPlan       :: Plan world c
    , _aiActionList :: [AIAction world c]
    , _aiPlanState  :: PlanState
} deriving Show

makeLenses ''AIState
makePrisms ''Plan
makePrisms ''AIAction

defaultAI :: AIState world c
defaultAI = AI {
      _aiPlan = PlanIdle
    , _aiActionList = []
    , _aiPlanState = PlanFinished
}
