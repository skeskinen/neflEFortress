{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Terrain where

import qualified Data.Vector as V
import Data.List.Split (chunksOf)
import Control.Lens
import Data.Vector.Lens
import Data.Maybe (fromMaybe)
import Tile

import Utils

data Terrain = Terrain { 
      _terrainWidth   :: Int
    , _terrainHeight  :: Int
    , _terrainDepth   :: Int
    , _terrainTiles   :: V.Vector Tile
}

makeLenses ''Terrain

instance Show Terrain where
    show t = concatMap ((++"\n").concatMap showPutLn) tileArray 
      where 
        showPutLn x = concatMap show x ++ "\n"
        tileArray = (map (chunksOf w).chunksOf (w*h)) (t ^. terrainTiles.from vector)
        w = t ^. terrainWidth
        h = t ^. terrainHeight

canMoveDir :: Terrain -> Point -> Dir -> Bool
canMoveDir terrain pos dir =
    if dir == DTop || dir == DBottom 
       then tileType1 == TileStairs && tileType2 == TileStairs
       else tileCanWalk tileType2
  where
    tileType1 = terrain ^. terrainTile pos . tileType
    tileType2 = terrain ^. terrainTile newPos . tileType
    newPos = addDir dir pos
indexTerrain :: Terrain -> Point -> Int
indexTerrain terrain (x, y, z) = x + y * w + z * w * h
  where
    w = view terrainWidth terrain
    h = view terrainHeight terrain

unindexTerrain :: Terrain -> Int -> Point
unindexTerrain terrain i = ( i `mod` w,(i `mod` (w * h)) `div` w, i `div` (w * h))
  where
    w = view terrainWidth terrain
    h = view terrainHeight terrain

getTerrainTile :: Point -> Terrain -> Tile
getTerrainTile pos terrain = terrain ^? terrainTiles . ix (indexTerrain terrain pos) & fromMaybe invalidTile

setTerrainTile :: Point -> Terrain -> Tile -> Terrain
setTerrainTile pos terrain tile = terrain & terrainTiles . ix (indexTerrain terrain pos) .~ tile

-- Note: not a real lens outside when passed invalid coordinates
terrainTile :: Point -> Lens' Terrain Tile
terrainTile pos = lens (getTerrainTile pos) (setTerrainTile pos)

getFloor :: Int -> Terrain -> Terrain
getFloor i t = Terrain w h 1 (V.slice start l (t ^. terrainTiles))
  where
    w = view terrainWidth t
    h = view terrainHeight t
    l = w*h
    start = l*i

toList :: Terrain -> [[Tile]]
toList t = chunksOf (t ^. terrainWidth)(t ^. (terrainTiles . from vector))
