{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Terrain where

import qualified Data.Vector as V
import qualified Data.IntSet as IS
import Control.Applicative ((<$>))
import Data.List.Split (chunksOf)
import Control.Lens
import Data.Vector.Lens
import Data.Maybe (fromMaybe)

import Utils

data TileType = 
      TileInvalid
    | TileEmpty
    | TileGround
    | TileWall
    deriving (Enum, Eq)

data Tile = Tile {
      _tileCreatures :: IS.IntSet
    , _tileItems     :: IS.IntSet
    , _tileType      :: TileType
} deriving (Eq)

data Terrain = Terrain { 
      _terrainWidth   :: Int
    , _terrainHeight  :: Int
    , _terrainDepth   :: Int
    , _terrainTiles   :: V.Vector Tile
}

makeLenses ''Tile
makeLenses ''Terrain

instance Show TileType where
    show TileInvalid = "%"
    show TileEmpty   = "+"
    show TileGround  = "."
    show TileWall    = "#"

tileTypeFromChar :: Char -> TileType
tileTypeFromChar '%' = TileInvalid
tileTypeFromChar '.' = TileGround
tileTypeFromChar '#' = TileWall
-- tileTypeFromChar '+' = TileEmpty
tileTypeFromChar _   = TileEmpty

instance Show Tile where
    show t 
      | (not . IS.null) $ t ^. tileCreatures = "@"
      | (not . IS.null) $ t ^. tileItems = "$"
      | otherwise = show $ t ^. tileType

instance Show Terrain where
    show t = concatMap ((++"\n").concatMap showPutLn) tileArray 
      where 
        showPutLn x = concatMap show x ++ "\n"
        tileArray = (map (chunksOf w).chunksOf (w*h)) (t ^. terrainTiles.from vector)
        w = t ^. terrainWidth
        h = t ^. terrainHeight

tileIsWall :: TileType -> Bool
tileIsWall TileEmpty  = False
tileIsWall TileGround = False
tileIsWall _          = True

tileCanWalk :: TileType -> Bool
tileCanWalk TileGround = True
tileCanWalk _          = False

invalidTile :: Tile
invalidTile = Tile {
      _tileCreatures = IS.empty
    , _tileType = TileInvalid
    , _tileItems = IS.empty
}

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

getTerrainTile :: Terrain -> Point -> Tile
getTerrainTile terrain pos = terrain ^? terrainTiles . ix (indexTerrain terrain pos) & fromMaybe invalidTile

setTerrainTile :: Terrain -> Point -> Tile -> Terrain
setTerrainTile terrain pos tile = terrain & terrainTiles . ix (indexTerrain terrain pos) .~ tile

-- Note: not a real lens outside when passed invalid coordinates
terrainTile :: Point -> Lens' Terrain Tile
terrainTile pos f terrain = setTerrainTile terrain pos <$> f (getTerrainTile terrain pos)

getFloor :: Terrain -> Int -> Terrain
getFloor t i = Terrain w h 1 (V.slice start length (t ^. terrainTiles))
  where
    w = view terrainWidth t
    h = view terrainHeight t
    length = w*h
    start = length*i

    
