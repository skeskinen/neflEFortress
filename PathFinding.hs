{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module PathFinding where

import Control.Lens
import qualified Data.Vector as V
import qualified Data.PQueue.Min as PQ

import Terrain
import Utils

data Node = Node {
      _nodePos :: !Point
    , _nodeFrom :: !(Maybe Node)
    , _nodeCost :: !Int
} deriving (Eq, Show)
    
makeLenses ''Node

instance Ord Node where
    a <= b = a ^. nodeCost <= b ^. nodeCost

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

