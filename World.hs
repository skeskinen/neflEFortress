{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State
import Control.Monad (when, (=<<))
import Control.Applicative
import Data.Foldable (foldMap)

import Terrain
import Item

type CreatureId = Int

data CreatureType = 
    CreatureNefle
  deriving (Enum, Eq)

data Creature = Creature {
      _creatureName  :: String
    , _creatureType  :: CreatureType
    , _creatureId    :: CreatureId
    , _creaturePos   :: (Int, Int, Int)
    , _creatureAct   :: Creature -> State World ()
    , _creatureAI    :: AI
    , _creatureItems :: [Item]
}

nameGenerator :: String
nameGenerator = "Uther"

data AI = AI {
      _aiState :: Int
}

data World = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap Creature
    , _worldItems     :: IM.IntMap Item
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
    show w = show (w ^. worldTerrain) 
            ++ showCreatures (w ^.. worldCreatures.traverse) 
            ++ showItems (w ^.. worldItems.traverse)
      where
        showCreatures = foldMap ((++"\n").show)
        showItems = foldMap ((++"\n").show)


creatureById :: CreatureId -> Lens' World (Maybe Creature)
creatureById i = worldCreatures . at i

worldPhysics :: State World ()
worldPhysics = do
    world <- get
    let creaturePhys creature = do
        let newpos = creature ^. creaturePos & _3 +~ 1
        if TileEmpty == world ^. worldTerrain . terrainTile newpos . tileType
            then fst <$> moveCreature creature newpos
            else return creature
    let itemPhys item = do
        let mnewpos = item ^? itemState . _ItemPos & traverse . _3 +~ 1
        case mnewpos of
            Just newpos ->
                if TileEmpty == world ^. worldTerrain . terrainTile newpos . tileType
                    then fst <$> moveItem item newpos
                    else return item
            Nothing ->
                return item
            
    newCreatures <- traverse creaturePhys (world ^. worldCreatures) 
    worldCreatures .= newCreatures

    newItems <- traverse itemPhys (world ^. worldItems) 
    worldItems .= newItems

stepWorld :: State World ()
stepWorld = do
    traverseM_ (use worldCreatures) (\creature -> (creature ^. creatureAct) creature)

    worldPhysics

addCreature :: Creature -> World -> World
addCreature creature world =
    world & worldMaxId +~ 1
          & worldCreatures . at newId ?~ newCreature
          & worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains newId .~ True
  where
    newId = world ^. worldMaxId + 1 
    newCreature = creature & creatureId .~ newId

addItem :: Item -> World -> World
addItem item world =
    world & worldMaxId +~ 1
          & worldItems . at newId ?~ newItem
          & case item ^. itemState of
            ItemPos pos  -> worldTerrain . terrainTile pos . tileItems . contains newId .~ True
            ItemHeldBy cid -> creatureById cid . traverse . creatureItems %~ (newItem :)
  where
    newId = world ^. worldMaxId + 1 
    newItem = item 
            & itemId .~ newId

pickUpItemId :: MonadState World m => ItemId -> CreatureId -> m ()
pickUpItemId itemid creatureid = do
    mitem <- use (worldItems . at itemid)
    mcreature <- use (worldCreatures . at creatureid)

    case (,) <$> mitem <*> mcreature of
        Just (item, creature) -> do
            forMOf_ (itemState . _ItemPos) item $ \pos -> worldTerrain . terrainTile pos . tileItems . contains itemid .= False
            let newItem = item & itemState .~ ItemHeldBy creatureid
            worldItems . at itemid .= Just newItem
            worldCreatures . at creatureid . traverse . creatureItems %= (newItem :)
        Nothing -> return ()

modifyCreature :: MonadState World m => CreatureId -> (Creature -> Creature) -> m ()
modifyCreature i f = creatureById i . traverse %= f

traverseM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
traverseM m f = m >>= mapMOf traverse f

traverseM_ :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m ()
traverseM_ m f = m >>= mapMOf_ traverse f

moveCreature :: MonadState World m => Creature -> Point -> m (Creature, Bool)
moveCreature creature pos = do
        t <- use $ worldTerrain . terrainTile pos . tileType
        if t == TileEmpty 
          then do
            let cid = creature ^. creatureId
            worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains cid .= False
            worldTerrain . terrainTile pos . tileCreatures . contains cid .= True
            return $ (creature & creaturePos .~ pos, True)
          else return (creature, False)

moveCreatureById :: MonadState World m => CreatureId -> Point -> m ()
moveCreatureById cid pos = traverseM_ (use (creatureById cid)) $ \creature -> do
        (_, wasMoved) <- moveCreature creature pos
        when wasMoved $ modifyCreature cid (creaturePos .~ pos)

moveItem :: MonadState World m => Item -> Point -> m (Item, Bool)
moveItem item pos = do
        t <- use $ worldTerrain . terrainTile pos . tileType
        if t == TileEmpty 
          then do
            let iid = item ^. itemId
            case item ^? itemState . _ItemPos of 
                Just oldpos -> worldTerrain . terrainTile oldpos . tileItems . contains iid .= False
                Nothing -> return ()
            worldTerrain . terrainTile pos . tileItems . contains iid .= True
            return $ (item & itemState . _ItemPos .~ pos, True)
          else return (item, False)

