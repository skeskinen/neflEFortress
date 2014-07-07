module WorldGenerating where 

import World
import Terrain
import Item

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.State


simpleWorld :: World
simpleWorld =  modifyWorld World {
      _worldTerrain = simpleTerrain
    , _worldCreatures = IM.empty
    , _worldMaxId = 0
    , _worldItems = IM.empty
}
  where
    modifyWorld world = world 
        & addCreature simpleCreature
        & addItem simpleItem

runSimpleWorld :: Int -> World
runSimpleWorld n = execState (replicateM_ n stepWorld) simpleWorld

simpleTerrain :: Terrain
simpleTerrain = modifyTerrain Terrain {
      _terrainWidth = 10
    , _terrainHeight = 10
    , _terrainDepth = 3
    , _terrainTiles = tiles
}
  where
    modifyTerrain terrain = terrain
        & terrainTile (5,4,0) . tileType .~ TileEmpty
        & terrainTile (5,4,1) . tileType .~ TileGround
        & terrainTile (3,3,0) . tileType .~ TileEmpty
        & terrainTile (3,3,1) . tileType .~ TileGround
    tiles = V.replicate 100 (tile TileGround) V.++ V.replicate 200 (tile TileWall)
    tile t = Tile {
          _tileCreatures = IS.empty
        , _tileItems = IS.empty
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
      _creatureName = nameGenerator
    , _creatureType = CreatureNefle
    , _creatureId = 1
    , _creaturePos = (5, 5, 0)
    , _creatureAct = simpleAct
    , _creatureAI = simpleAI
    , _creatureItems = []
}

simpleItem :: Item
simpleItem = Item {
      _itemId = 0
    , _itemType = Bed
    , _itemMaterial = Iron
    , _itemState = ItemPos (3,3,0) 
}
