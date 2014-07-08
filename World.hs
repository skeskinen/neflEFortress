{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Data.Foldable (foldMap)

import Terrain
import Item
import Utils

type CreatureId = Int

data CreatureType = 
    CreatureNefle
  deriving (Enum, Eq)

data Creature s = Creature {
      _creatureName  :: String
    , _creatureType  :: CreatureType
    , _creatureId    :: CreatureId
    , _creaturePos   :: (Int, Int, Int)
    , _creatureAct   :: Creature s -> State (World s) ()
    , _creatureState :: s
    , _creatureItems :: [Item]
}

nameGenerator :: String
nameGenerator = "Uther"

data (World cs) = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap (Creature cs)
    , _worldItems     :: IM.IntMap Item
    , _worldMaxId     :: Int
}

makeLenses ''World
makeLenses ''Creature

instance Show CreatureType where
    show CreatureNefle = "Nefle"

instance (Show a) => Show (Creature a) where
    show c = (c ^. creatureName) ++ "; " ++ show (c ^. creatureType) ++ "; " 
      ++ show (c ^. creaturePos) 
      ++ "; Carried items: " ++ show (lengthOf (creatureItems . traverse) c) ++ ", " 
      ++ show (c ^. creatureItems) ++ "; " ++ show (c ^. creatureState)

instance (Show cs) => Show (World cs) where
    show w = show (w ^. worldTerrain) 
            ++ showCreatures (w ^.. worldCreatures.traverse) 
            ++ showItems (w ^.. worldItems.traverse)
      where
        showCreatures = foldMap ((++"\n").show)
        showItems = foldMap ((++"\n").show)

creatureById :: CreatureId -> Lens' (World cs) (Maybe (Creature cs))
creatureById i = worldCreatures . at i

worldPhysics :: State (World cs) ()
worldPhysics = do
    world <- get
    let phys objPos moveObj obj = do
        let mpos = obj ^? objPos
        case mpos of
            Just pos -> do
                let newpos = pos & _3 +~ 1
                let getTileType position = world ^. worldTerrain . terrainTile position . tileType
                if TileEmpty == getTileType pos && TileWall /= getTileType newpos 
                    then fst <$> moveObj obj newpos
                    else return obj
            Nothing -> 
                return obj
            
    newCreatures <- traverse (phys creaturePos moveCreature) (world ^. worldCreatures) 
    worldCreatures .= newCreatures

    newItems <- traverse (phys (itemState . _ItemPos) moveItem) (world ^. worldItems) 
    worldItems .= newItems

stepWorld :: State (World cs) ()
stepWorld = do
    traverseM_ (use worldCreatures) (\creature -> (creature ^. creatureAct) creature)

    worldPhysics

addCreature :: Creature cs -> World cs -> World cs
addCreature creature world =
    world & worldMaxId +~ 1
          & worldCreatures . at newId ?~ newCreature
          & worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains newId .~ True
  where
    newId = world ^. worldMaxId + 1 
    newCreature = creature & creatureId .~ newId

addItem :: Item -> World cs -> World cs
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
            
getItemPosId :: MonadState (World cs) m => ItemId -> m (Maybe Point)
getItemPosId iid = use . pre $ worldItems . at iid . traverse . itemState . _ItemPos

pickUpItemId :: MonadState (World cs) m => ItemId -> Creature cs -> m (Creature cs)
pickUpItemId itemid creature = do
    mitem <- use (worldItems . at itemid)
    case mitem of 
        Just item -> do
            forMOf_ (itemState . _ItemPos) item $ \pos -> worldTerrain . terrainTile pos . tileItems . contains itemid .= False
            let newItem = item & itemState .~ ItemHeldBy (creature ^. creatureId)
            worldItems . at itemid .= Just newItem

            return $ creature & creatureItems %~ (newItem :)
        Nothing -> return creature

modifyCreature :: MonadState (World cs) m => CreatureId -> (Creature cs -> Creature cs) -> m ()
modifyCreature i f = creatureById i . traverse %= f

traverseM :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m (t b)
traverseM m f = m >>= mapMOf traverse f

traverseM_ :: (Traversable t, Monad m) => m (t a) -> (a -> m b) -> m ()
traverseM_ m f = m >>= mapMOf_ traverse f

moveCreature :: MonadState (World cs) m => Creature s -> Point -> m (Creature s , Bool)
moveCreature creature pos = do
        t <- use $ worldTerrain . terrainTile pos . tileType
        if not (tileIsWall t)
          then do
            let cid = creature ^. creatureId
            worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains cid .= False
            worldTerrain . terrainTile pos . tileCreatures . contains cid .= True
            return (creature & creaturePos .~ pos, True)
          else return (creature, False)

moveItem :: MonadState (World cs) m => Item -> Point -> m (Item, Bool)
moveItem item pos = do
        t <- use $ worldTerrain . terrainTile pos . tileType
        if not (tileIsWall t)
          then do
            let iid = item ^. itemId
            case item ^? itemState . _ItemPos of 
                Just oldpos -> worldTerrain . terrainTile oldpos . tileItems . contains iid .= False
                Nothing -> return ()
            worldTerrain . terrainTile pos . tileItems . contains iid .= True
            return (item & itemState . _ItemPos .~ pos, True)
          else return (item, False)

