{-# LANGUAGE TemplateHaskell #-} 
module Test.TerrainTests where

import Terrain

import Test.QuickCheck
import Test.QuickCheck.All
import Data.DeriveTH

import qualified Data.Vector as V
import qualified Data.IntSet as IS
import Control.Applicative ((<$>))
import Data.List.Split (chunksOf)
import Control.Lens
import Data.Vector.Lens
import Data.Maybe (fromMaybe)
import Control.Applicative

derive makeArbitrary ''TileType

instance Arbitrary Tile where
    arbitrary = do
        tileType <- arbitrary
        return (Tile IS.empty IS.empty tileType)

instance Arbitrary Terrain where
    arbitrary = do
        w <- smi
        h <- smi
        d <- smi
        let size = w*h*d
        tiles <- V.fromList <$> vectorOf size arbitrary 
        return (Terrain w h d tiles)
        where
          smi = choose (3,7)

randomTerrainPoint :: Terrain -> Gen Point
randomTerrainPoint t = do
    x <- choose (0,t ^. terrainWidth - 1)
    y <- choose (0,t ^. terrainHeight - 1)
    z <- choose (0,t ^. terrainDepth - 1)
    return (x,y,z)

prop_inBounds terrain = forAll (randomTerrainPoint terrain) $ \p ->
    indexTerrain terrain p < V.length (terrain ^. terrainTiles)

prop_indexUnwind terrain = forAll (randomTerrainPoint terrain) $ \p -> 
    unindexTerrain terrain (indexTerrain terrain p) == p

prop_tilePutGet terrain = forAll (randomTerrainPoint terrain) $ \p ->
    forAll arbitrary $ \tile ->
        view (terrainTile p) (set (terrainTile p) tile terrain) == tile

return []
terrainTests = $quickCheckAll

