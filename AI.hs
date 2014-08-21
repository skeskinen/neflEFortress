{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AI where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Foldable (asum)
import Data.IntSet.Lens
import Data.Maybe (isJust)

import Building
import Item
import PathFinding
import Terrain
import Utils
import World
import Creature

import AIState

tryDig :: Creature -> Terrain -> Point -> Maybe (Point, [AIAction World Creature])
tryDig creature terrain point
    | isJust (tile ^. tileReserved) = Nothing
    | otherwise = 
      case tile ^. tileType of
        TileWall _ -> do
            let tryDigging dir = do
                    let digPos = addDir dir point
                    if tileIsWall (terrain ^. terrainTile digPos . tileType)
                        then Nothing
                        else (\dirs -> [AIMove dirs, AIDig (reverseDir dir)]) <$> 
                                findPath terrain (creature ^. creaturePos) digPos
            (\a -> (point, a)) <$> (asum $ map tryDigging [DUp, DDown, DRight, DLeft])
        _ -> Nothing
  where
    tile = terrain ^. terrainTile point

isReserved :: Point -> State World Bool
isReserved point = isJust <$> use (worldTerrain . terrainTile point . tileReserved)

reserveTile :: Creature -> Point -> State World ()
reserveTile creature point = 
    worldTerrain . terrainTile point . tileReserved .= 
        (Just . getObjId $ creature ^. creatureId) 

unreserveTile :: Point -> State World ()
unreserveTile point = 
    worldTerrain . terrainTile point . tileReserved .= Nothing

makeActions :: Creature -> State World AI
makeActions creature = case ai ^. aiPlanState of
    PlanStarted -> case ai ^. aiPlan of
        PlanIdle -> return $ ai
                            & aiActionList
                                .~  [ AIIdle
                                    , AIMove [DRight, DDown, DLeft, DUp]
                                    , AIIdle]
                            & aiPlanState .~ PlanStarted
        PlanPickUpItem iid -> do
            mitemPos <- getObjPosId iid 
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
                Just (point, actions) -> do
                    reserveTile creature point
                    return $ 
                      ai  & aiActionList .~ actions
                          & aiPlanState .~ PlanStarted
                Nothing -> doNothing
        PlanBuild bid -> tryBuild bid >>= doOrDoNothing
        _ -> doNothing
    PlanFinished -> do 
        let tryJobs jobs = case jobs of
                [] -> idle
                (job:moreJobs) -> case job of
                    JobDig area -> do
                        terrain <- use worldTerrain
                        let points = areaPoints area
                        case asum $ map (tryDig creature terrain) points of
                            Just (point, actions) -> do
                                reserveTile creature point
                                return $
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
        allJobs <- use worldJobs
        tryJobs allJobs 
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
        bpos <- preuse $ objLens bid . traverse . buildingPos

        res <- traverse isReserved bpos

        case res of 
            Just False -> do
                mpath <- preuse $ objLens bid 
                                        . traverse 
                                        . buildingPos 
                                        . to (findPath terrain (creature ^. creaturePos))
                                        . traverse
                case mpath of 
                    Just path -> do
                        traverse (reserveTile creature) bpos
                        return . Just $
                            ai & aiActionList .~ [AIMove path, AIBuild]
                               & aiPlanState .~ PlanStarted
                               & aiPlan .~ PlanOther
                    Nothing -> return Nothing
            _ -> return Nothing

runAI :: Creature -> State World Creature
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
                    (newCreature, wasMoved) <- moveObjDir creature dir
                    let newActions = if wasMoved 
                                        then AIMove dirs : as
                                        else []
                    return $ setActionList newActions newCreature
                AIPickUpItem iid -> do
                    mitemPos <- getObjPosId iid 
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
                    let pos = addDir dir (creature ^. creaturePos)
                    tile <- worldTerrain . terrainTile pos . tileType <%= dig
                    case tile of 
                         TileWall _ -> return creature
                         _ -> do
                            worldTerrain . terrainTile pos . tileReserved .= Nothing
                            nextAction
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
                            bState <- use . pre $ worldBuildings . at bid . traverse . buildingState 
                            if isn't (_Just . _BuildingBuilding) bState
                               then do
                                   unreserveTile (creature ^. creaturePos)
                                   nextAction
                               else return creature
                        Nothing -> nextAction

                _ -> return $ setActionList as creature
        

