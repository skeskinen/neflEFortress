{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module World where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Control.Applicative ((<$>))

import Terrain

type CreatureId = Int

data CreatureType = 
	CreatureNefle
  deriving Enum

data Creature = Creature {
	  _creatureType :: CreatureType
	, _creatureId   :: CreatureId
	, _creaturePos  :: (Int, Int, Int)
	, _creatureAct  :: CreatureId -> State World ()
}

data World = World {
	  _worldTerrain   :: Terrain
	, _worldCreatures :: IM.IntMap Creature
	, _worldMaxId	  :: Int
}

makeLenses ''World
makeLenses ''Creature

creatureById :: CreatureId -> Lens' World Creature
creatureById i = worldCreatures . imLens
  where
	imLens f k = (\v -> IM.insert i v k) <$> f (k IM.! i)

stepWorld :: State World ()
stepWorld = do
	creatures <- IM.toList <$> use worldCreatures
	mapM_ (\(i, creature) -> (creature ^. creatureAct) i) creatures

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

addCreature :: Creature -> World -> World
addCreature creature world =
	world & worldMaxId +~ 1
		  & worldCreatures %~ IM.insert newId newCreature
		  & worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures %~ (newId :)
  where
	newId = world ^. worldMaxId + 1 
	newCreature = creature & creatureId .~ newId

simpleWorld :: World
simpleWorld = addCreature simpleCreature $ World {
	  _worldTerrain = simpleTerrain
	, _worldCreatures = IM.empty
	, _worldMaxId = 0
}
