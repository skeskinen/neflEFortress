{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
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
import Creature
import AIState

type Creature = CreatureP World 
type Building = BuildingP World
type Item = ItemP World Creature

type ItemId = ItemIdP World Creature
type BuildingId = BuildingIdP World
type CreatureId = CreatureIdP World 

type AI = AIState World Creature

data World = World {
      _worldTerrain   :: Terrain
    , _worldCreatures :: IM.IntMap Creature
    , _worldItems     :: IM.IntMap Item
    , _worldMaxId     :: Int
    , _worldJobs      :: [Job]
    , _worldBuildings :: IM.IntMap Building
}

data Job = 
    JobDig Area
  | JobBuild BuildingId
  | JobBrew

makeLenses ''World

class WorldObject world obj | obj -> world where
    objLens       :: ObjId obj -> Lens' world (Maybe obj)
    objId         :: obj -> ObjId obj
    objPos        :: Traversal' obj Point
    tileContains  :: Point -> ObjId obj -> Lens' world Bool

instance WorldObject World (CreatureP World) where
    objLens i = worldCreatures . at (getObjId i)
    objId = view creatureId
    objPos = creaturePos
    tileContains p oid = worldTerrain . terrainTile p . tileCreatures . contains (getObjId oid)

instance WorldObject World (BuildingP World) where
    objLens i = worldBuildings . at (getObjId i)
    objId = view buildingId
    objPos = buildingPos
    tileContains p oid = worldTerrain . terrainTile p . tileBuildings . contains (getObjId oid)

instance WorldObject World (ItemP World Creature) where
    objLens i = worldItems . at (getObjId i)
    objId = view itemId
    objPos = itemState . _ItemPos
    tileContains p oid = worldTerrain . terrainTile p . tileItems . contains (getObjId oid)

instance Show Job where
    show (JobDig area) = "Dig area " ++ show area
    show (JobBuild bid) = "Building " ++ show bid
    show JobBrew = "Brew"

instance Show World where
    show w = show (w ^. worldTerrain) 
            ++ showObjs (w ^.. worldCreatures.traverse) 
            ++ showObjs (w ^.. worldItems.traverse)
            ++ showObjs (w ^.. worldBuildings.traverse)
            ++ showObjs (w ^.. worldJobs.traverse)
      where
        showObjs :: Show a => [a] -> String
        showObjs = foldMap ((++"\n").show)

worldPhysics :: State World ()
worldPhysics = do
    world <- get
    let phys obj = do
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
            
    newCreatures <- traverse phys (world ^. worldCreatures) 
    worldCreatures .= newCreatures

    newItems <- traverse phys (world ^. worldItems) 
    worldItems .= newItems

jobFinished :: Job -> State World Bool
jobFinished (JobDig area) = do
    terrain <- use worldTerrain
    return . and $ map (\pos -> not . tileIsWall $ terrain ^. terrainTile pos . tileType) (areaPoints area)
jobFinished (JobBuild bid) = do
    bState <- use . pre $ objLens bid . traverse . buildingState 
    return $ isn't (_Just . _BuildingBuilding) bState
jobFinished JobBrew = return False

checkJobs :: State World ()
checkJobs = do
    jobs <- use worldJobs
    newJobs <- filterM (\job -> not <$> jobFinished job) jobs
    worldJobs .= newJobs

stepWorld :: State World ()
stepWorld = do
    creatures <- use worldCreatures
    for_ creatures $ \creature -> 
        (creature ^. creatureAct) creature

    buildings <- use worldBuildings
    for_ buildings $ \building ->
        (building ^. buildingAct) building

    checkJobs
    worldPhysics

startBuilding :: BuildingType -> Point -> World -> World
startBuilding bt pos world = 
    world & worldMaxId +~ 1
          & objLens newId ?~ b
          & worldJobs %~ (JobBuild newId :)
          & tileContains pos newId .~ True
  where
    newId :: BuildingId
    newId = ObjId $ world ^. worldMaxId + 1 
    b = makeBuilding bt
        & buildingPos .~ pos
        & buildingId .~ newId

addCreature :: Creature -> World -> World
addCreature creature world =
    world & worldMaxId +~ 1
          & objLens newId ?~ newCreature
          & tileContains (creature ^. creaturePos) newId .~ True
  where
    newId :: CreatureId
    newId = ObjId $ world ^. worldMaxId + 1 
    newCreature = creature & creatureId .~ newId

addItem :: Item -> World -> World
addItem item world =
    world & worldMaxId +~ 1
          & objLens newId ?~ newItem
          & case item ^. itemState of
            ItemPos pos  -> tileContains pos newId .~ True
            ItemHeldBy cid -> objLens cid . traverse . creatureItems %~ (newItem :)
  where
    newId :: ItemId
    newId = ObjId $ world ^. worldMaxId + 1 
    newItem = item 
            & itemId .~ newId
            
getObjPosId :: (MonadState world m, WorldObject world object) => ObjId object -> m (Maybe Point)
getObjPosId oid = preuse $ objLens oid . traverse . objPos

pickUpItemId :: MonadState World m => ItemId -> Creature -> m Creature
pickUpItemId itemid creature = do
    mitem <- use (objLens itemid)
    case mitem of 
        Just item -> do
            forMOf_ objPos item $ \pos -> tileContains pos itemid .= False
            let newItem = item & itemState .~ ItemHeldBy (creature ^. creatureId)
            objLens itemid ?= newItem

            return $ creature & creatureItems %~ (newItem :)
        Nothing -> return creature

moveObjDir :: (MonadState World m, WorldObject World obj) => obj -> Dir -> m (obj, Bool)
moveObjDir obj dir = do
    terrain <- use worldTerrain
    case (\x -> (canMoveDir terrain x dir, x)) <$> obj ^? objPos of
      Just (True, oldPos) -> do
        let pos = addDir dir oldPos
        newObj <- moveObj obj pos
        return (newObj, True)
      _ -> return (obj, False)

moveObj :: (MonadState world m, WorldObject world obj) => obj -> Point -> m obj
moveObj obj pos = do
    let oid = objId obj
    forMOf_ objPos obj $ \opos -> tileContains opos oid .= False
    tileContains pos oid .= True
    return $ obj & objPos .~ pos

makeBuilding :: BuildingType -> Building
makeBuilding BuildingField = 
    defBuilding & buildingType .~ BuildingField
                & buildingAct .~ fieldAct
                & buildingInternal .~ Just (FieldTimer 10)
makeBuilding BuildingBrewery = 
    defBuilding & buildingType .~ BuildingBrewery

fieldAct :: Building -> State World ()
fieldAct field = 
    when (hasn't (buildingState . _BuildingBuilding) field) $
        if anyOf (buildingInternal . _Just . _FieldTimer) (<= 0) field 
            then do
                modify $ addItem Item {
                      _itemId = ObjId (-1)
                    , _itemType = Wheat
                    , _itemMaterial = None
                    , _itemState = ItemPos (field ^. buildingPos)
                }
                fieldTimer .= 10
           else fieldTimer -= 1
  where
    fieldTimer = objLens (objId field) . _Just . buildingInternal . _Just . _FieldTimer 
