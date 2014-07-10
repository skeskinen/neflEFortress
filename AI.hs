{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AI where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.IntSet.Lens

import Building
import Item
import PathFinding
import Terrain
import Utils
import World

data Plan = 
    PlanPickUpItem ItemId
  | PlanMoveTo Point
  | PlanDig Point
  | PlanBuild BuildingId
  | PlanIdle
  | PlanOther
  deriving Show

data PlanState =
    PlanStarted
  | PlanFinished
  deriving Show

data AIAction = 
    AIPickUpItem ItemId
  | AIMove [Dir]
  | AIDig Dir
  | AIBuild
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
        PlanBuild bid -> tryBuild bid >>= doOrDoNothing
        _ -> doNothing
    PlanFinished -> do 
        worldJobs <- use worldJobs
        let tryJobs jobs = case jobs of
                [] -> idle
                (job:moreJobs) -> case job of
                    JobDig area -> do
                        terrain <- use worldTerrain
                        let points = areaPoints area
                        case asum $ map (tryDig creature terrain) points of
                             Just actions -> return $
                                                ai & aiActionList .~ actions
                                                   & aiPlanState .~ PlanStarted 
                                                   & aiPlan .~ PlanOther
                             Nothing -> tryJobs moreJobs
                    JobBuild bid -> do
                        mbuild <- tryBuild bid
                        case mbuild of
                             Just build -> return build
                             Nothing -> tryJobs moreJobs
                    _ -> tryJobs moreJobs
                    
        tryJobs worldJobs
  where
    ai = creature ^. creatureState
    doNothing = return $ ai & aiPlanState .~ PlanFinished
    idle = return $
                ai & aiPlan .~ PlanIdle
                   & aiPlanState .~ PlanStarted
    doOrDoNothing (Just b) = return b
    doOrDoNothing Nothing  = doNothing

    tryBuild bid = do
        terrain <- use worldTerrain
        mpath <- use . pre $ worldBuildings 
                                . at bid 
                                . traverse 
                                . buildingPos 
                                . to (findPath terrain (creature ^. creaturePos))
                                . traverse
        return $ mpath <&> \path -> 
                    ai & aiActionList .~ [AIMove path, AIBuild]
                       & aiPlanState .~ PlanStarted
                       & aiPlan .~ PlanOther

runAI :: Creature AI -> State (World AI) (Creature AI)
runAI creature = do
    let setActionList as = creatureState . aiActionList .~ as
    case creature ^. creatureState . aiActionList of 
        [] -> do 
            actions <- makeActions creature 
            runAI (creature & creatureState .~ actions)
        (a:as) -> 
            let nextAction = return $ setActionList as creature
            in case a of 
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
                                    nextAction
                        Nothing -> nextAction 
                AIDig dir -> do
                    let dig tile = case tile of
                            TileWall i -> if i > 0
                                             then TileWall (i - 1)
                                             else TileGround
                            t -> t
                    tile <- worldTerrain . terrainTile (addDir dir (creature ^. creaturePos)) . tileType <%= dig
                    case tile of 
                         TileWall _ -> return creature
                         _ -> nextAction
                AIBuild -> do
                    let build tile = case tile of
                            BuildingBuilding i -> 
                                if i > 0
                                   then BuildingBuilding (i - 1)
                                   else BuildingReady
                            t -> t
                    mbid <- use . pre $ worldTerrain . terrainTile (creature ^. creaturePos) . tileBuildings . members
                    case mbid of
                        Just bid -> do
                            worldBuildings . at bid . traverse . buildingState %= build
                            state <- use . pre $ worldBuildings . at bid . traverse . buildingState 
                            if isn't (_Just . _BuildingBuilding) state
                               then nextAction
                               else return creature
                        Nothing -> nextAction

                _ -> return $ setActionList as creature
        

