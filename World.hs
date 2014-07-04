{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Lens
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
    , _worldMaxId      :: Int
}

makeLenses ''World
makeLenses ''Creature

creatureById :: CreatureId -> Lens' World (Maybe Creature)
creatureById i = worldCreatures . at i

stepWorld :: State World ()
stepWorld = do
    creatures <- IM.toList <$> use worldCreatures
    mapM_ (\(i, creature) -> (creature ^. creatureAct) i) creatures


addCreature :: Creature -> World -> World
addCreature creature world =
    world & worldMaxId +~ 1
          & worldCreatures %~ IM.insert newId newCreature
          & worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures %~ (newId :)
  where
    newId = world ^. worldMaxId + 1 
    newCreature = creature & creatureId .~ newId

modifyCreature :: MonadState World m => CreatureId -> (Creature -> Creature) -> m ()
modifyCreature i f = do
    creatureById i . traverse %= f

