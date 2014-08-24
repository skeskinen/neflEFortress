{-# LANGUAGE Arrows, ScopedTypeVariables, TypeFamilies #-}
module Ui 
    ( GameWire
    , UiImpl(..)
    , UiWorld(..)
    , runUiImpl 
    , module X
    ) where

import Control.Monad.Reader as X (Reader, ask, asks, runReader)
import Control.Monad.State
--import Control.Monad
import Control.Lens

import Prelude hiding ((.), id, until)
import Control.Wire as X
import FRP.Netwire as X

import Utils as X
import UiUtils as X
import World 
import Terrain
import Tile as X
import WorldGenerating

type GameWire impl = Wire (Timed NominalDiffTime ()) () (Reader (Input impl)) 
type MWire m = Wire (Timed NominalDiffTime ()) () m 

class UiImpl impl where 
    data Input  impl :: *
    data Output impl :: *
    --data GameState  impl :: *

    handleInput     :: impl -> IO (Maybe (Input impl, impl))
    handleOutput    :: (Output impl, impl) -> IO ()     
    sleep           :: impl -> IO ()     

    outputFreq      :: GameWire impl    a          (Event a) 
    worldFreq       :: GameWire impl    a          (Event a) 
    processWorld    :: GameWire impl    UiWorld    (Output impl) 
    coMoveCursorRel :: GameWire impl    a          (Event (Dir, Int)) 
    coDig           :: GameWire impl    a          (Event ())

data UiWorld = UiWorld { tiles :: [[Tile]], focus :: Point2 }
simpleUiWorld :: UiWorld
simpleUiWorld = UiWorld [] (0,0)
                                     

runUiImpl :: forall impl. (UiImpl impl) => impl -> IO () 
runUiImpl s0 = run s0 clockSession_ mainWire 
  where
    run s1 session mainWire' = do
        (dt, newSession) <- stepSession session
        r <- handleInput s1
        case r of 
            Just i@(_, s2) -> do
                (_, newMainWire) <- stepWire mainWire' dt (Right i)
                sleep s2
                run s2 newSession newMainWire
            Nothing -> return ()

    mainWire :: MWire IO (Input impl, impl) ()
    mainWire = proc (input, uiState) -> do
        rec
            runWorld  <- liftWire worldFreq -< (world, input)
            runOutput <- liftWire outputFreq -< (world, input)

            world <- execOnce mkId step . first (delay simpleWorld) -< (world, runWorld)

        let mergedEvent = runWorld `mergeL` runOutput
        uiWorld <- hold . accumE generateUiWorld simpleUiWorld -< mergedEvent 
        o <- liftWire processWorld -< (uiWorld, input)
        
        execOnce (mkConst (Right ())) outputWire -< ((o, uiState), runOutput) 
        
    liftWire :: GameWire impl a b -> MWire IO (a, Input impl) b --Maybe find better way to do this?
    liftWire w = WGen $ \ s (Right (a, i)) -> do --UGH such pattern match
        let (b, w') = runReader (stepWire w s (Right a)) i --inhibit not handled correctly
        return (b, liftWire w')

    generateUiWorld :: UiWorld -> World -> UiWorld
    generateUiWorld prev world = 
        let curFloor = getFloor 1 (world ^. worldTerrain)
            --drawnTiles = foldWithIndices drawTile [] (toList curFloor) 
            --focus = [drawImage 3 0 A.Focus]
        in UiWorld (toList curFloor) (0,0)

    outputWire :: MWire IO (Output impl, impl) ()
    outputWire = mkGen_ (fmap Right . handleOutput) 

    step :: MWire IO World World
    step = arr (execState stepWorld)

{-execOnce (arr snd) go . (mkId &&& periodic 0.02)
        returnA -< output
    glGame :: GameWire impl World (World, Output GLUi)
    glGame = proc world' -> do
        world <- execOnce mkId (arr (execState stepWorld)) . second (periodic 0.1) -< (world', ()) 
        rec
            x <- integral 0 -< 1 
            y <- (deltaT (\ds a -> 2 * ds * a) . dir) + delay 11 -< y
            let pelle = drawColored x y A.Creature3 yellow


        let rque = drawnTiles ++ focus ++ [pelle]
            output = GLOutput rque
        keyUp Key'Q -< (world, output)
      where
        --dir = keyDown Key'Up . (-0.0001) <|> keyDown Key'Down . 0.0001 <|> 0 
        dir = keyDown Key'Up . (-1) <|> keyDown Key'Down . 1 <|> 0 -}

