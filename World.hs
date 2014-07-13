{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module World where

import qualified Data.IntMap as IM
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.State
import Data.Foldable (foldMap, for_)

import Building
import Item
import Terrain
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

data World cs = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap (Creature cs)
    , _worldItems     :: IM.IntMap Item
    , _worldMaxId     :: Int
    , _worldJobs      :: [Job]
    , _worldBuildings :: IM.IntMap (Building (World cs))
}

data Job = 
    JobDig Area
  | JobBuild BuildingId
  | JobBrew

makeLenses ''World
makeLenses ''Creature

instance Show CreatureType where
    show CreatureNefle = "Nefle"

instance (Show a) => Show (Creature a) where
    show c = (c ^. creatureName) ++ "; " ++ show (c ^. creatureType) ++ "; " 
      ++ show (c ^. creaturePos) 
      ++ "; Carried items: " ++ show (lengthOf (creatureItems . traverse) c) ++ ", " 
      ++ show (c ^. creatureItems) ++ "; " ++ show (c ^. creatureState)

instance Show Job where
    show (JobDig area) = "Dig area " ++ show area
    show (JobBuild bid) = "Building " ++ show bid
    show JobBrew = "Brew"

instance (Show cs) => Show (World cs) where
    show w = show (w ^. worldTerrain) 
            ++ showObjs (w ^.. worldCreatures.traverse) 
            ++ showObjs (w ^.. worldItems.traverse)
            ++ showObjs (w ^.. worldBuildings.traverse)
            ++ showObjs (w ^.. worldJobs.traverse)
      where
        showObjs :: Show a => [a] -> String
        showObjs = foldMap ((++"\n").show)

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
                    if TileEmpty == getTileType pos && not (tileIsWall (getTileType newpos))
                        then moveObj obj newpos
                        else return obj
                Nothing -> 
                    return obj
            
    newCreatures <- traverse (phys creaturePos moveCreature) (world ^. worldCreatures) 
    worldCreatures .= newCreatures

    newItems <- traverse (phys (itemState . _ItemPos) moveItem) (world ^. worldItems) 
    worldItems .= newItems

jobFinished :: Job -> State (World cs) Bool
jobFinished (JobDig area) = do
    terrain <- use worldTerrain
    return . and $ map (\pos -> not . tileIsWall $ terrain ^. terrainTile pos . tileType) (areaPoints area)
jobFinished (JobBuild bid) = do
    bState <- use . pre $ worldBuildings . at bid . traverse . buildingState 
    return $ isn't (_Just . _BuildingBuilding) bState
jobFinished JobBrew = return False

checkJobs :: State (World cs) ()
checkJobs = do
    jobs <- use worldJobs
    newJobs <- filterM (\job -> not <$> jobFinished job) jobs
    worldJobs .= newJobs

stepWorld :: State (World cs) ()
stepWorld = do
    creatures <- use worldCreatures
    for_ creatures $ \creature -> 
        (creature ^. creatureAct) creature

    buildings <- use worldBuildings
    for_ buildings $ \building ->
        (building ^. buildingAct) building

    checkJobs
    worldPhysics

startBuilding :: BuildingType -> Point -> World cs -> World cs
startBuilding bt pos world = 
    world & worldMaxId +~ 1
          & worldBuildings . at newId ?~ b
          & worldJobs %~ (JobBuild newId :)
          & worldTerrain . terrainTile pos . tileBuildings . contains newId .~ True
  where
    newId = world ^. worldMaxId + 1 
    b = makeBuilding bt
        & buildingPos .~ pos
        & buildingId .~ newId

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
            worldItems . at itemid ?= newItem

            return $ creature & creatureItems %~ (newItem :)
        Nothing -> return creature

moveCreatureDir :: MonadState (World cs) m => Creature cs -> Dir -> m (Creature cs, Bool)
moveCreatureDir creature dir = do
    terrain <- use worldTerrain
    if canMoveDir terrain (creature ^. creaturePos) dir
      then do
        let pos = addDir dir (creature ^. creaturePos)
        newCreature <- moveCreature creature pos
        return (newCreature, True)
      else return (creature, False)

moveCreature :: MonadState (World cs) m => Creature cs -> Point -> m (Creature cs)
moveCreature creature pos = do
    let cid = creature ^. creatureId
    worldTerrain . terrainTile (creature ^. creaturePos) . tileCreatures . contains cid .= False
    worldTerrain . terrainTile pos . tileCreatures . contains cid .= True
    return $ creature & creaturePos .~ pos

moveItem :: MonadState (World cs) m => Item -> Point -> m Item
moveItem item pos = do
    let iid = item ^. itemId
    case item ^? itemState . _ItemPos of 
        Just oldpos -> worldTerrain . terrainTile oldpos . tileItems . contains iid .= False
        Nothing -> return ()
    worldTerrain . terrainTile pos . tileItems . contains iid .= True
    return $ item & itemState . _ItemPos .~ pos

makeBuilding :: BuildingType -> Building (World cs)
makeBuilding BuildingField = 
    defBuilding & buildingType .~ BuildingField
                & buildingAct .~ fieldAct
                & buildingInternal .~ Just (FieldTimer 10)
makeBuilding BuildingBrewery = 
    defBuilding & buildingType .~ BuildingBrewery

fieldAct :: Building (World cs) -> State (World cs) ()
fieldAct field = 
    when (hasn't (buildingState . _BuildingBuilding) field) $
        if anyOf (buildingInternal . _Just . _FieldTimer) (<= 0) field 
            then do
                modify $ addItem Item {
                      _itemId = -1
                    , _itemType = Wheat
                    , _itemMaterial = None
                    , _itemState = ItemPos (field ^. buildingPos)
                }
                worldBuildings . at (field ^. buildingId) . _Just . buildingInternal . _Just . _FieldTimer .= 10
           else worldBuildings . at (field ^. buildingId) . _Just . buildingInternal . _Just . _FieldTimer -= 1
