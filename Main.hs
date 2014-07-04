module Main where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.State

import World
import Terrain


simpleWorld :: World
simpleWorld = addCreature simpleCreature World {
      _worldTerrain = simpleTerrain
    , _worldCreatures = IM.empty
    , _worldMaxId = 0
}

simpleTerrain :: Terrain
simpleTerrain = Terrain {
      _terrainWidth = 10
    , _terrainHeight = 10
    , _terrainDepth = 1
    , _terrainTiles = V.replicate 100 tile
}
  where
    tile = Tile {
        _tileCreatures = []
        , _tileType = TileEmpty
    }

simpleAI :: AI
simpleAI = AI 0


simpleAct :: CreatureId -> State World ()
simpleAct cid = traverseM_ (use (creatureById cid)) $ \creature -> do 
    let dir = creature ^. creatureAI . aiState
    let coor = [_1, _2] !! (dir `mod` 2)
    let diff = 2 * (dir `div` 2) - 1
    moveCreature cid ((creature ^. creaturePos) & coor +~ diff) 
    modifyCreature cid (creatureAI . aiState %~ (\i -> (i + 1) `mod` 4))


simpleCreature :: Creature 
simpleCreature = Creature {
      _creatureType = CreatureNefle
    , _creatureId = 1
    , _creaturePos = (5, 5, 0)
    , _creatureAct = simpleAct
    , _creatureAI = simpleAI
}

