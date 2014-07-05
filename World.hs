{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State
import Control.Monad (when, (=<<))
import Control.Applicative

import Terrain

type CreatureId = Int

data CreatureType = 
    CreatureNefle
  deriving (Enum, Eq)

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

worldPhysics :: State World ()
worldPhysics = do
    world <- get
    let creaturePhys cid creature = do
        let newpos = creature ^. creaturePos & _3 +~ 1
        when (TileEmpty == world ^. worldTerrain . terrainTile newpos . tileType) $ do
            moveCreature cid newpos
    
    itraverse_ creaturePhys (world ^. worldCreatures) 

stepWorld :: State World ()
stepWorld = do
    creatures <- IM.toList <$> use worldCreatures
    mapM_ (\(i, creature) -> (creature ^. creatureAct) i) creatures

    worldPhysics


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
        t <- use $ worldTerrain . terrainTile pos . tileType
        when (t == TileEmpty) $ do
            worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures %= filter (/= cid)
            worldTerrain . terrainTile pos . tileCreatures %= (cid :)
            modifyCreature cid (creaturePos .~ pos)

