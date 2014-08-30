{-# LANGUAGE Arrows, ScopedTypeVariables, TypeFamilies, FlexibleContexts, RankNTypes #-}
module Ui 
    ( MWire
    , GameWire
    , UiImpl(..)
    , UiWorld(..)
    , runUiImpl 
    , module X
    ) where

import Control.Monad.Reader as X (Reader, ask, asks, runReader)
import Control.Monad.State
import Data.Maybe
import Control.Lens
import qualified Data.IntSet as IS

import Prelude hiding ((.), id, until)
import Control.Wire as X
import FRP.Netwire as X
import Control.Wire.Unsafe.Event as X (Event(..))

import Utils as X
import UiUtils as X
import World 
import Terrain
import Tile as X
import Item as X
import Building as X
import WorldGenerating

--import Debug.Trace

type GameWire impl = Wire (Timed NominalDiffTime ()) () (Reader (Input impl)) 
type MWire m = Wire (Timed NominalDiffTime ()) () m 

class UiImpl impl where 
    data Input  impl :: *
    data Output impl :: *
    --data GameState  impl :: *
    type GameIO impl :: * -> *
    type GameIO impl = IO

    handleInput     :: impl -> (GameIO impl) (Maybe (Input impl, impl))
    handleOutput    :: (Output impl, impl) -> (GameIO impl) ()     
    sleep           :: impl -> (GameIO impl) ()     
    outputFreq      :: impl -> MWire (GameIO impl) a (Event a)

    worldFreq       :: GameWire impl    a           (Event a) 
    processWorld    :: GameWire impl    UiWorld     (Output impl) 
    coQuit          :: GameWire impl    a           (Event a)
    coMoveCursorRel :: GameWire impl    a           (Event (Dir, Int)) 
    coStartSelect   :: GameWire impl    a           (Event a)
    coCancelSelect  :: GameWire impl    a           (Event a)
    coDig           :: GameWire impl    a           (Event a)

data UiWorld = UiWorld { 
    tiles     :: [[Tile]],
    creatures :: [Creature],
    buildings :: [Building],
    items     :: [Item],
    focusPos  :: Point,
    selectPos :: Maybe Point
    }

runUiImpl :: forall impl. (UiImpl impl, MonadIO (GameIO impl), Applicative (GameIO impl)) => 
    impl -> (GameIO impl) () 
runUiImpl s0 = run s0 clockSession_ mainWire 
  where
    run s1 session mainWire' = do
        (dt, newSession) <- stepSession session
        r <- handleInput s1
        case r of 
            Just (input, s2) -> do
                let (o, newMainWire) = runReader (stepWire mainWire' dt (Right ())) input
                whenRight o (\ o' -> do
                    _ <- stepWire (outputWire s2) dt (Right o')
                    sleep s2
                    run s2 newSession newMainWire)
            Nothing -> return ()

    outputWire :: impl -> MWire (GameIO impl) (Output impl) ()
    outputWire s = proc output -> 
        execOnceSet (execOutput s) (mkConst (Right ())) . leftId (outputFreq s) -< output

    execOutput :: impl -> MWire (GameIO impl) (Output impl) ()
    execOutput s = mkGen_ (\ o -> Right <$> handleOutput (o, s))

data UiState = UiState { s_world :: World, s_focus :: Point, s_select :: Maybe Point}
    
mainWire :: UiImpl impl => GameWire impl () (Output impl)
mainWire = proc _ -> do
    runWorld  <- worldFreq -< ()
    rec
        world <- modifyWorld . execOnceSet step mkId -< (s, runWorld)
        focus <- moveCursor . leftId coMoveCursorRel -< s
        select <- selectCancelWire . leftId coCancelSelect . selectWire . leftId coStartSelect -< s
        s <- delay (UiState simpleWorld (2,2,0) Nothing) -< UiState world focus select
    uiWorld <- generateUiWorld -< s

    until . leftId coQuit . processWorld -< uiWorld

selectWire :: UiImpl impl => GameWire impl (UiState, Event a) (Maybe Point)
selectWire = execOnceSet (arr (\s -> Just (s_focus s))) (arr (\s -> s_select s))

selectCancelWire :: UiImpl impl => GameWire impl (Maybe Point, Event a) (Maybe Point) 
selectCancelWire = execOnceSet (mkConst (Right Nothing)) mkId

moveCursor :: UiImpl impl => GameWire impl (UiState, Event (Dir, Int)) Point 
moveCursor = execOnceMap boundsWire (arr s_focus) 

boundsWire :: UiImpl impl => (Dir, Int) -> GameWire impl UiState Point
boundsWire (dir, dist) = arr (\ (UiState{s_world=w, s_focus=f}) -> bounds (addDirI dir dist f) w)
  where
    bounds :: Point -> World -> Point
    bounds p w = boundPoint p ((0,0,0), w ^. worldTerrain . terrainSize)

step :: UiImpl impl => GameWire impl UiState UiState
step = arr (\s@UiState{s_world=w} -> s{s_world = execState stepWorld w})

modifyWorld :: UiImpl impl => GameWire impl UiState World
modifyWorld = proc s0 -> do
    s1 <- execOnceSet digWire mkId . leftId coDig -< s0
    returnA -< s_world s1

digWire :: UiImpl impl => GameWire impl UiState UiState 
digWire = arr (\ (s@UiState{s_world = w, s_focus = f, s_select = sel}) -> 
                    s{s_world=w & worldJobs %~ (digJob f sel :)})
  where
    digJob :: Point -> Maybe Point -> Job
    digJob p Nothing = JobDig (p, p)
    digJob p (Just sel) = JobDig (boundingBox (sel, p))

generateUiWorld :: UiImpl impl => GameWire impl UiState UiWorld
generateUiWorld = proc (UiState{s_world=world, s_focus=focus, s_select=select}) -> do
    let curFloor   = getFloor (focus ^. _3) (world ^. worldTerrain)
        tileList   = toList curFloor
        creatures' = getObjs world tileList tileCreatures
        buildings' = getObjs world tileList tileBuildings
        items'     = getObjs world tileList tileItems
    returnA -< UiWorld tileList creatures' buildings' items' focus select
  where
    getObjs :: (WorldObject World a) => World -> [[Tile]] -> Lens' Tile IS.IntSet -> [a]
    getObjs world tileList l = map (fromJust . (world ^.) . objLens . ObjId) . 
                              IS.toList $ (tileList ^. traverse . traverse . l)
    
