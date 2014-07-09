{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module AI where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Foldable (asum)
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V

import World
import Item
import Terrain
import Utils

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

data Node = Node {
      _nodePos :: !Point
    , _nodeFrom :: !(Maybe Node)
    , _nodeCost :: !Int
} deriving (Eq, Show)
    

makeLenses ''AI
makeLenses ''Node
makePrisms ''Plan
makePrisms ''AIAction

instance Ord Node where
    a <= b = a ^. nodeCost <= b ^. nodeCost

defaultAI :: AI
defaultAI = AI {
      _aiPlan = PlanIdle
    , _aiActionList = []
    , _aiPlanState = PlanFinished
}

findPath :: Terrain -> Point -> Point -> Maybe [Dir]
findPath terrain start end = visit (PQ.singleton $ Node start Nothing 0) noneVisited
  where
    distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

    getPath (Just node) path = getPath (node ^. nodeFrom) (node : path)
    getPath Nothing path = path

    visit pq visited = 
        case PQ.minView pq of
             Just (node@(Node pos _ cost), newPq) -> 
                let 
                    canMove dir = Just True /= visited ^? ix (indexTerrain terrain newPos)
                            && canMoveDir terrain pos dir
                      where newPos = addDir dir pos
                    newCost newPos = 1 + cost - distance pos end + distance newPos end
                    addNode dir curPq = 
                        if canMove dir 
                            then Node newPos (Just node) (newCost newPos) `PQ.insert` curPq
                            else curPq
                      where newPos = addDir dir pos
                    
                    updatePq = foldr addNode newPq [minBound .. maxBound]

                in if pos == end 
                    then pointsToDirs $ map (^. nodePos) $ getPath (Just node) []
                    else visit updatePq (visited & ix (indexTerrain terrain pos) .~ True)
             Nothing -> Nothing
    noneVisited = V.replicate (terrain ^. terrainTiles . to V.length) False

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
            case terrain ^. terrainTile point . tileType of
                TileWall _ -> do
                    let tryDigging dir = do
                        let digPos = addDir dir point
                        if tileIsWall (terrain ^. terrainTile digPos . tileType)
                            then Nothing
                            else (\dirs -> [AIMove dirs, AIDig (reverseDir dir)]) <$> 
                                findPath terrain (creature ^. creaturePos) digPos

                    let mactions = asum $ map tryDigging [DUp, DDown, DRight, DLeft]
                    case mactions of
                        Just actions -> return $ ai
                                            & aiActionList .~ actions
                                            & aiPlanState .~ PlanStarted
                        Nothing -> doNothing
                _ -> doNothing
            
    PlanFinished -> return $ ai 
                            & aiPlan .~ PlanIdle
                            & aiPlanState .~ PlanStarted
  where
    ai = creature ^. creatureState
    doNothing = return $ ai
                        & aiPlanState .~ PlanFinished

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

