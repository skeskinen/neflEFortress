module WorldGenerating where 

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.State

import AI
import Building
import Item
import Terrain
import Utils
import World

type World' = World AI
type Creature' = Creature AI

simpleWorld :: World'
simpleWorld =  modifyWorld World {
      _worldTerrain = simpleTerrain
    , _worldCreatures = IM.empty
    , _worldMaxId = 0
    , _worldItems = IM.empty
    , _worldJobs = [JobDig ((6, 1, 1), (7, 5, 1))]
    , _worldBuildings = IM.empty
}
  where
    modifyWorld world = world 
        & addCreature simpleCreature
        & addCreature simpleCreature
        & addItem (simpleItem (3, 3, 0))
        & addItem (simpleItem (2, 6, 0))
        & addItem (simpleItem (8, 1, 1))
        & addItem (simpleItem (1, 1, 1))
        & startBuilding BuildingField   (6, 1, 1)
        & startBuilding BuildingBrewery (6, 2, 1)


runSimpleWorld :: Int -> World'
runSimpleWorld n = execState (replicateM_ n stepWorld) simpleWorld

simpleTerrain :: Terrain
simpleTerrain = modifyTerrain Terrain {
      _terrainWidth = 10
    , _terrainHeight = 10
    , _terrainDepth = 2
    , _terrainTiles = tiles
}
  where
    modifyTerrain terrain = terrain
    tiles = tileFloor1 V.++ tileFloor2 
    tileFloor1 = makeLevel [
          "##########"
        , "#........#"
        , "#........#"
        , "#..+.....#"
        , "#....+...#"
        , "####...#x#"
        , "#..#####.#"
        , "#..#...#.#"
        , "#....#...#"
        , "##########"
        ]
    tileFloor2 = makeLevel [
          "##########"
        , "#.....##.#"
        , "#.....##.#"
        , "#.....##.#"
        , "#.....##.#"
        , "####..##x#"
        , "#..#####.#"
        , "#..#...#.#"
        , "#....#.#.#"
        , "##########"
        ]
    makeLevel = V.fromList . concatMap (map (tile . tileTypeFromChar))
    tile t = Tile {
          _tileCreatures = IS.empty
        , _tileItems = IS.empty
        , _tileType = t
        , _tileBuildings = IS.empty
        , _tileReserved = Nothing
    }

simpleAI :: AI
simpleAI = defaultAI 
            & aiPlan .~ PlanPickUpItem 4
--            & aiPlan .~ PlanDig (7, 3, 1)
            & aiPlanState .~ PlanStarted

simpleAct :: Creature' -> State World' ()
simpleAct creature = do 
    newCreature <- runAI creature
    worldCreatures . at (creature ^. creatureId) . traverse .= newCreature 

simpleCreature :: Creature' 
simpleCreature = Creature {
      _creatureName = nameGenerator
    , _creatureType = CreatureNefle
    , _creatureId = 1
    , _creaturePos = (5, 3, 0)
    , _creatureAct = simpleAct
    , _creatureState = simpleAI
    , _creatureItems = []
}

simpleItem :: Point -> Item
simpleItem pos = Item {
      _itemId = 0
    , _itemType = Bed
    , _itemMaterial = Iron
    , _itemState = ItemPos pos
}

