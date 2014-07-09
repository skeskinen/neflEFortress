module WorldGenerating where 

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.State

import AI
import World
import Terrain
import Item
import Utils

type World' = World AI
type Creature' = Creature AI

simpleWorld :: World'
simpleWorld =  modifyWorld World {
      _worldTerrain = simpleTerrain
    , _worldCreatures = IM.empty
    , _worldMaxId = 0
    , _worldItems = IM.empty
}
  where
    modifyWorld world = world 
        & addCreature simpleCreature
        & addItem (simpleItem (3, 3, 0))
        & addItem (simpleItem (2, 6, 0))
        & addItem (simpleItem (8, 1, 1))
        & addItem (simpleItem (1, 1, 1))

findSimplePath :: Point -> Point -> Maybe [Dir]
findSimplePath = findPath simpleTerrain

runSimpleWorld :: Int -> World'
runSimpleWorld n = execState (replicateM_ n stepWorld) simpleWorld

simpleTerrain :: Terrain
simpleTerrain = modifyTerrain Terrain {
      _terrainWidth = 10
    , _terrainHeight = 10
    , _terrainDepth = 3
    , _terrainTiles = tiles
}
  where
    modifyTerrain terrain = terrain
    tiles = tileFloor1 V.++ tileFloor2 V.++ V.replicate 100 (tile $ TileWall 3)
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
        , "#......#.#"
        , "#......#.#"
        , "#......#.#"
        , "#......#.#"
        , "####...#x#"
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
    }

simpleAI :: AI
simpleAI = defaultAI 
--            & aiPlan .~ PlanPickUpItem 4
            & aiPlan .~ PlanDig (7, 3, 1)
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

