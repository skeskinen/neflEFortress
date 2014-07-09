{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AI where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Foldable (asum)

import World
import Item
import Terrain
import Utils
import PathFinding

data Plan = 
    PlanPickUpItem ItemId
  | PlanMoveTo Point
  | PlanDig Point
  | PlanIdle
  deriving Show

data PlanState =
    PlanStarted
  | PlanFinished
  deriving Show

data AIAction = 
    AIPickUpItem ItemId
  | AIMove [Dir]
  | AIDig Dir
  | AIIdle 
  deriving Show

data AI = AI {
      _aiPlan       :: Plan
    , _aiActionList :: [AIAction]
    , _aiPlanState  :: PlanState
} deriving Show

makeLenses ''AI
makePrisms ''Plan
makePrisms ''AIAction

defaultAI :: AI
defaultAI = AI {
      _aiPlan = PlanIdle
    , _aiActionList = []
    , _aiPlanState = PlanFinished
}

tryDig :: Creature AI -> Terrain -> Point -> Maybe [AIAction]
tryDig creature terrain point = 
    case terrain ^. terrainTile point . tileType of
        TileWall _ -> do
            let tryDigging dir = do
                let digPos = addDir dir point
                if tileIsWall (terrain ^. terrainTile digPos . tileType)
                    then Nothing
                    else (\dirs -> [AIMove dirs, AIDig (reverseDir dir)]) <$> 
                            findPath terrain (creature ^. creaturePos) digPos
            asum $ map tryDigging [DUp, DDown, DRight, DLeft]
        _ -> Nothing

makeActions :: Creature AI -> State (World AI) AI
makeActions creature = case ai ^. aiPlanState of
    PlanStarted -> case ai ^. aiPlan of
        PlanIdle -> return $ ai
                            & aiActionList
                                .~  [ AIIdle
                                    , AIMove [DRight, DDown, DLeft, DUp]
                                    , AIIdle]
                            & aiPlanState .~ PlanStarted
        PlanPickUpItem iid -> do
            mitemPos <- getItemPosId iid 
            case mitemPos of 
                 Just itemPos -> do
                     terrain <- use worldTerrain
                     case findPath terrain (creature ^. creaturePos) itemPos of
                          Just path -> return $ ai
                                        & aiActionList .~ [AIMove path, AIPickUpItem iid]
                                        & aiPlanState .~ PlanStarted
                          Nothing -> doNothing
                 Nothing -> doNothing
        PlanMoveTo pos -> do
            terrain <- use worldTerrain
            case findPath terrain (creature ^. creaturePos) pos of
                 Just path -> return $ ai
                                & aiActionList .~ [AIMove path]
                                & aiPlanState .~ PlanStarted
                 Nothing -> doNothing
        PlanDig point -> do
            terrain <- use worldTerrain
            let mactions = tryDig creature terrain point
            case mactions of
                Just actions -> return $ 
                    ai  & aiActionList .~ actions
                        & aiPlanState .~ PlanStarted
                Nothing -> doNothing
            
    PlanFinished -> do 
        jobs <- use worldJobs
        case jobs of
            [] -> idle
            (job:_) -> case job of
                JobDig area -> do
                    terrain <- use worldTerrain
                    let points = areaPoints area
                    case asum $ map (tryDig creature terrain) points of
                         Just actions -> return $
                                            ai & aiActionList .~ actions
                                               & aiPlanState .~ PlanStarted 
                         Nothing -> idle
  where
    ai = creature ^. creatureState
    doNothing = return $ ai & aiPlanState .~ PlanFinished
    idle = return $
                ai & aiPlan .~ PlanIdle
                   & aiPlanState .~ PlanStarted

runAI :: Creature AI -> State (World AI) (Creature AI)
runAI creature = do
    let setActionList as = creatureState . aiActionList .~ as
    case creature ^. creatureState . aiActionList of 
        [] -> do 
            actions <- makeActions creature 
            runAI (creature & creatureState .~ actions)
        (a:as) -> 
            case a of 
                AIMove (dir:dirs) -> do
                    (newCreature, wasMoved) <- moveCreatureDir creature dir
                    let newActions = if wasMoved 
                                        then AIMove dirs : as
                                        else []
                    return $ setActionList newActions newCreature
                AIPickUpItem iid -> do
                    mitemPos <- getItemPosId iid 
                    case mitemPos of 
                        Just pos -> 
                            if pos == creature ^. creaturePos
                                then do
                                   newCreature <- pickUpItemId iid creature
                                   return $ setActionList as newCreature
                                else 
                                   return $ setActionList as creature
                        Nothing -> return $ setActionList as creature
                AIDig dir -> do
                    let dig tile = case tile of
                            TileWall i -> if i > 0
                                             then TileWall (i - 1)
                                             else TileGround
                            t -> t
                    tile <- worldTerrain . terrainTile (addDir dir (creature ^. creaturePos)) . tileType <%= dig
                    case tile of 
                         TileWall _ -> return creature
                         _ -> return $ setActionList as creature
                _ -> return $ setActionList as creature

