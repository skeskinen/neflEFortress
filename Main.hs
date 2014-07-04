module Main where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
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

simpleCreature :: Creature 
simpleCreature = Creature {
      _creatureType = CreatureNefle
    , _creatureId = 1
    , _creaturePos = (5, 5, 0)
    , _creatureAct = const $ return ()
}
