{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State
import Control.Applicative

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
    , _creatureAI   :: AI
}

data AI = AI {
      _aiState :: Int
}

data World = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap Creature
    , _worldMaxId      :: Int
}

makeLenses ''World
makeLenses ''Creature
makeLenses ''AI

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
modifyCreature i f = creatureById i . traverse %= f

traverseM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
traverseM m f = m >>= mapMOf traverse f

traverseM_ :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m ()
traverseM_ m f = traverseM m f >> return ()

moveCreature :: MonadState World m => CreatureId -> (Int, Int, Int) -> m ()
moveCreature cid pos = traverseM_ (use (creatureById cid)) $ \creature -> do
        -- TODO: add border checking (to terrainTile?)
        worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures %= filter (/= cid)
        worldTerrain . terrainTile pos . tileCreatures %= (cid :)
        modifyCreature cid (creaturePos .~ pos)

