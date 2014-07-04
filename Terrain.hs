{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Terrain where

import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Control.Lens

data TileType = 
      TileEmpty
    | TileGround
    deriving Enum

data Tile = Tile {
      _tileCreatures :: [Int]
    , _tileType        :: TileType
}


data Terrain = Terrain { 
      _terrainWidth   :: Int
    , _terrainHeight  :: Int
    , _terrainDepth      :: Int
    , _terrainTiles   :: V.Vector Tile
}


makeLenses ''Tile
makeLenses ''Terrain

indexTerrain :: Terrain -> (Int, Int, Int) -> Int
indexTerrain terrain (x, y, z) = x + y * w + z * w * h
  where
    w = view terrainWidth terrain
    h = view terrainHeight terrain

getTerrainTile :: Terrain -> (Int, Int, Int) -> Tile
getTerrainTile terrain pos = view terrainTiles terrain V.! indexTerrain terrain pos

setTerrainTile :: Terrain -> (Int, Int, Int) -> Tile -> Terrain
setTerrainTile terrain pos tile = terrain & terrainTiles %~ (V.// [(indexTerrain terrain pos, tile)]) 

terrainTile :: (Int, Int, Int) -> Lens' Terrain Tile
terrainTile pos f terrain = setTerrainTile terrain pos <$> f (getTerrainTile terrain pos)

