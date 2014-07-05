module Main where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
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

runSimpleWorld :: Int -> World
runSimpleWorld n = execState (replicateM_ n stepWorld) simpleWorld

simpleTerrain :: Terrain
simpleTerrain = modify Terrain {
      _terrainWidth = 10
    , _terrainHeight = 10
    , _terrainDepth = 3
    , _terrainTiles = tiles
}
  where
    modify terrain = terrain
        & terrainTile (5,4,1) . tileType .~ TileEmpty
    tiles = V.replicate 100 (tile TileEmpty) V.++ V.replicate 200 (tile TileGround)
    tile t = Tile {
          _tileCreatures = IS.empty
        , _tileType = t
    }

simpleAI :: AI
simpleAI = AI 0

simpleAct :: Creature -> State World ()
simpleAct creature = do
    let dir = creature ^. creatureAI . aiState
    let coor = [_1, _2] !! (dir `mod` 2)
    let diff = 2 * (dir `div` 2) - 1
    let cid = creature ^. creatureId
    moveCreatureById cid ((creature ^. creaturePos) & coor +~ diff) 
    modifyCreature cid (creatureAI . aiState %~ (\i -> (i + 1) `mod` 4))


simpleCreature :: Creature 
simpleCreature = Creature {
      _creatureType = CreatureNefle
    , _creatureId = 1
    , _creaturePos = (5, 5, 0)
    , _creatureAct = simpleAct
    , _creatureAI = simpleAI
}

