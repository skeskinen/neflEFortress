module WorldGenerating where 

import World
import Terrain
import Item

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.State
import AI


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
        & terrainTile (5,4,1) . tileType .~ TileGround
        & terrainTile (3,3,1) . tileType .~ TileGround
    tiles = tileFloor V.++ V.replicate 200 (tile TileWall)
    tileFloor = makeLevel [
          "##########"
        , "#........#"
        , "#........#"
        , "#..+.....#"
        , "#....+...#"
        , "####...#.#"
        , "#..#####.#"
        , "#..#...#.#"
        , "#....#...#"
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
            & aiPlan .~ PlanPickUpItem 3
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

