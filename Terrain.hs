{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Terrain where

import qualified Data.Vector as V
import qualified Data.IntSet as IS
import Data.List.Split (chunksOf)
import Control.Lens
import Data.Vector.Lens
import Data.Maybe (fromMaybe)

import Utils

data TileType = 
      TileInvalid
    | TileEmpty
    | TileGround
    | TileWall Int
    | TileStairs
    deriving Eq

-- TODO: merge the intsets to one set
data Tile = Tile {
      _tileCreatures :: IS.IntSet
    , _tileItems     :: IS.IntSet
    , _tileBuildings :: IS.IntSet
    , _tileType      :: TileType
} deriving Eq

data Terrain = Terrain { 
      _terrainWidth   :: Int
    , _terrainHeight  :: Int
    , _terrainDepth   :: Int
    , _terrainTiles   :: V.Vector Tile
}

makeLenses ''Tile
makeLenses ''Terrain

instance Show TileType where
    show TileInvalid  = "%"
    show TileEmpty    = "+"
    show TileGround   = "."
    show (TileWall _) = "#"
    show TileStairs   = "x"

tileTypeFromChar :: Char -> TileType
tileTypeFromChar '%' = TileInvalid
tileTypeFromChar '.' = TileGround
tileTypeFromChar '#' = TileWall 3
-- tileTypeFromChar '+' = TileEmpty
tileTypeFromChar 'x' = TileStairs
tileTypeFromChar _   = TileEmpty

instance Show Tile where
    show t 
      | (not . IS.null) $ t ^. tileCreatures = "@"
      | (not . IS.null) $ t ^. tileItems = "$"
      | (not . IS.null) $ t ^. tileBuildings = "O"
      | otherwise = show $ t ^. tileType

instance Show Terrain where
    show t = concatMap ((++"\n").concatMap showPutLn) tileArray 
      where 
        showPutLn x = concatMap show x ++ "\n"
        tileArray = (map (chunksOf w).chunksOf (w*h)) (t ^. terrainTiles.from vector)
        w = t ^. terrainWidth
        h = t ^. terrainHeight

tileIsWall :: TileType -> Bool
tileIsWall TileInvalid  = True
tileIsWall (TileWall _) = True
tileIsWall _            = False

tileCanWalk :: TileType -> Bool
tileCanWalk TileGround = True
tileCanWalk TileStairs = True
tileCanWalk _          = False

canMoveDir :: Terrain -> Point -> Dir -> Bool
canMoveDir terrain pos dir =
    if dir == DTop || dir == DBottom 
       then tileType1 == TileStairs && tileType2 == TileStairs
       else tileCanWalk tileType2
  where
    tileType1 = terrain ^. terrainTile pos . tileType
    tileType2 = terrain ^. terrainTile newPos . tileType
    newPos = addDir dir pos

invalidTile :: Tile
invalidTile = Tile {
      _tileCreatures = IS.empty
    , _tileType = TileInvalid
    , _tileItems = IS.empty
    , _tileBuildings = IS.empty
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

getTerrainTile :: Point -> Terrain -> Tile
getTerrainTile pos terrain = terrain ^? terrainTiles . ix (indexTerrain terrain pos) & fromMaybe invalidTile

setTerrainTile :: Point -> Terrain -> Tile -> Terrain
setTerrainTile pos terrain tile = terrain & terrainTiles . ix (indexTerrain terrain pos) .~ tile

-- Note: not a real lens outside when passed invalid coordinates
terrainTile :: Point -> Lens' Terrain Tile
terrainTile pos = lens (getTerrainTile pos) (setTerrainTile pos)

getFloor :: Terrain -> Int -> Terrain
getFloor t i = Terrain w h 1 (V.slice start length (t ^. terrainTiles))
  where
    w = view terrainWidth t
    h = view terrainHeight t
    length = w*h
    start = length*i

    
