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

import Debug.Trace

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
    coDig           :: GameWire impl    a           (Event a)

data UiWorld = UiWorld { 
    tiles     :: [[Tile]],
    creatures :: [Creature],
    buildings :: [Building],
    items     :: [Item],
    focusPos  :: Point 
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
        execOnceSet (execOutput s) (mkConst (Right ())) . (mkId &&& outputFreq s) -< output

    execOutput :: impl -> MWire (GameIO impl) (Output impl) ()
    execOutput s = mkGen_ (\ o -> Right <$> handleOutput (o, s))

    mainWire :: GameWire impl () (Output impl)
    mainWire = proc _ -> do
        runWorld  <- worldFreq -< ()
        rec
            world  <- execOnceSet step mkId . first (delay simpleWorld) -< (world', runWorld)
            world' <- modifyWorld -< (world, focus)
            focus <- moveCursor coMoveCursorRel . first (delay (0,4,0)) -< (focus, world')
        uiWorld <- generateUiWorld -< (world', focus)

        quitEvent <- coQuit -< ()
        until . first processWorld -< (uiWorld, quitEvent)

    step :: GameWire impl World World
    step = arr (execState stepWorld)

    modifyWorld :: GameWire impl (World, Point) World
    modifyWorld = proc (world0, focus) -> do
        digEvent <- coDig -< ()
        world1 <- execOnceSet digWire (arr fst) -< ((world0, focus), digEvent)
        returnA -< world1

    digWire :: GameWire impl (World, Point) World
    digWire = arr (\ (w, f) -> traceShow (w ^. worldJobs) (w & worldJobs %~ (digJob f :)))

    digJob :: Point -> Job
    digJob p@(x,y,z) = JobDig (p, (x+1,y+1,z))

    generateUiWorld :: GameWire impl (World, Point) UiWorld
    generateUiWorld = proc (world, focus) -> do
        let curFloor   = getFloor (focus ^. _3) (world ^. worldTerrain)
            tileList   = toList curFloor
            creatures' = getObjs world tileList tileCreatures
            buildings' = getObjs world tileList tileBuildings
            items'     = getObjs world tileList tileItems
        returnA -< UiWorld tileList creatures' buildings' items' focus

    getObjs :: (WorldObject World a) => World -> [[Tile]] -> Lens' Tile IS.IntSet -> [a]
    getObjs world tileList l = map (fromJust . (world ^.) . objLens . ObjId) . 
                              IS.toList $ (tileList ^. traverse . traverse . l)
    
    moveCursor :: GameWire impl (Point, World) (Event (Dir, Int)) -> 
                    GameWire impl (Point, World) Point
    moveCursor eventWire = execOnceMap boundsWire (arr fst) . (mkId &&& eventWire)

    boundsWire :: (Dir, Int) -> GameWire impl (Point, World) Point
    boundsWire (dir, dist) = arr (\ (p, w) -> bounds (addDirI dir dist p) w)

    bounds :: Point -> World -> Point
    bounds p w = boundPoint p ((0,0,0), w ^. worldTerrain . terrainSize)
