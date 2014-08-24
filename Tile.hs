{-# LANGUAGE TemplateHaskell #-}
module Tile where

import qualified Data.IntSet as IS
import Control.Lens

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
    , _tileReserved  :: Maybe Int
} deriving Eq

makeLenses ''Tile

instance Show TileType where
    show TileInvalid  = "%"
    show TileEmpty    = "+"
    show TileGround   = "."
    show (TileWall _) = "#"
    show TileStairs   = "x"

instance Show Tile where
    show t 
      | (not . IS.null) $ t ^. tileCreatures = "@"
      | (not . IS.null) $ t ^. tileItems = "$"
      | (not . IS.null) $ t ^. tileBuildings = "O"
      | otherwise = show $ t ^. tileType

tileTypeFromChar :: Char -> TileType
tileTypeFromChar '%' = TileInvalid
tileTypeFromChar '.' = TileGround
tileTypeFromChar '#' = TileWall 3
-- tileTypeFromChar '+' = TileEmpty
tileTypeFromChar 'x' = TileStairs
tileTypeFromChar _   = TileEmpty

tileIsWall :: TileType -> Bool
tileIsWall TileInvalid  = True
tileIsWall (TileWall _) = True
tileIsWall _            = False

tileCanWalk :: TileType -> Bool
tileCanWalk TileGround = True
tileCanWalk TileStairs = True
tileCanWalk _          = False

invalidTile :: Tile
invalidTile = Tile {
      _tileCreatures = IS.empty
    , _tileType = TileInvalid
    , _tileItems = IS.empty
    , _tileBuildings = IS.empty
    , _tileReserved = Nothing
}

