{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State
import Control.Monad (when, (=<<))
import Control.Applicative
import Data.Foldable (foldMap)
import Terrain

type CreatureId = Int

data CreatureType = 
    CreatureNefle
  deriving (Enum, Eq)

data Creature = Creature {
      _creatureName :: String
    , _creatureType :: CreatureType
    , _creatureId   :: CreatureId
    , _creaturePos  :: (Int, Int, Int)
    , _creatureAct  :: Creature -> State World ()
    , _creatureAI   :: AI
}

nameGenerator :: String
nameGenerator = "Uther"

data AI = AI {
      _aiState :: Int
}

data World = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap Creature
    , _worldMaxId     :: Int
}

makeLenses ''World
makeLenses ''Creature
makeLenses ''AI

instance Show CreatureType where
    show CreatureNefle = "Nefle"

instance Show Creature where
    show c = (c ^. creatureName) ++ ", " ++ show (c ^. creatureType) ++ " " ++ show (c ^. creaturePos) 

instance Show World where
    show w = show (w ^. worldTerrain) ++ showCreatures (w ^.. worldCreatures.traverse)
      where
        showCreatures = foldMap ((++"\n").show)

creatureById :: CreatureId -> Lens' World (Maybe Creature)
creatureById i = worldCreatures . at i

worldPhysics :: State World ()
worldPhysics = do
    world <- get
    let creaturePhys creature = do
        let newpos = creature ^. creaturePos & _3 +~ 1
        if TileEmpty == world ^. worldTerrain . terrainTile newpos . tileType
            then moveCreature creature newpos
            else return creature
    
    (worldCreatures .=) =<< traverse creaturePhys (world ^. worldCreatures) 

stepWorld :: State World ()
stepWorld = do
    creatures <- IM.toList <$> use worldCreatures
    mapM_ (\(i, creature) -> (creature ^. creatureAct) creature) creatures

    worldPhysics


addCreature :: Creature -> World -> World
addCreature creature world =
    world & worldMaxId +~ 1
          & worldCreatures %~ IM.insert newId newCreature
          & worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains newId .~ True
  where
    newId = world ^. worldMaxId + 1 
    newCreature = creature & creatureId .~ newId

modifyCreature :: MonadState World m => CreatureId -> (Creature -> Creature) -> m ()
modifyCreature i f = creatureById i . traverse %= f

traverseM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
traverseM m f = m >>= mapMOf traverse f

traverseM_ :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m ()
traverseM_ m f = traverseM m f >> return ()

moveCreature :: MonadState World m => Creature -> Point -> m Creature
moveCreature creature pos = do
        t <- use $ worldTerrain . terrainTile pos . tileType
        if t == TileEmpty 
          then do
            let cid = creature ^. creatureId
            worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains cid .= False
            worldTerrain . terrainTile pos . tileCreatures . contains cid .= True
            return $ creature & creaturePos .~ pos
          else return creature

moveCreatureById :: MonadState World m => CreatureId -> Point -> m ()
moveCreatureById cid pos = traverseM_ (use (creatureById cid)) $ \creature -> do
        t <- use $ worldTerrain . terrainTile pos . tileType
        when (t == TileEmpty) $ do
            worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains cid .= False
            worldTerrain . terrainTile pos . tileCreatures . contains cid .= True
            modifyCreature cid (creaturePos .~ pos)

