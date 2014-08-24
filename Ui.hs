{-# LANGUAGE Arrows, ScopedTypeVariables, TypeFamilies #-}
module Ui where

import Control.Monad.Reader
--import Control.Monad
--import Control.Lens

import Prelude hiding ((.), id, until)
import Control.Wire
--import FRP.Netwire

--import Utils
import World
--import Terrain
import WorldGenerating

type IOWire a b = Wire (Timed NominalDiffTime ()) () IO a b
type GameWire impl = Wire (Timed NominalDiffTime ()) () (Reader (PureInput impl)) 

class UiImpl impl where 
    data PureInput  impl :: *
    data PureOutput impl :: *
    data GameState  impl :: *
    --type GameWire impl   :: * -> * -> *

    beforeFrame     :: IOWire        impl 
                                     (PureInput impl, impl)

    afterFrame      :: IOWire        (PureOutput impl, impl) 
                                     impl

    gameWire        :: GameWire impl World 
                                     (World, PureOutput impl)

    --coMoveCursorRel :: impl -> 

runUiImpl :: forall impl. (UiImpl impl) => impl -> IO () 
runUiImpl impl = do
    let bw = beforeFrame
        aw = afterFrame
    run impl clockSession_ mainWire bw aw
  where
    run s1 session wire beforeWire afterWire = do
        (dt, session') <- stepSession session
        (s2', bw) <- stepWire beforeWire dt (Right s1)
        whenRight s2' $ \ (input, s2) -> do
            let (output', w) = runReader (stepWire wire dt (Right () )) input
            whenRight output' $ \ output -> do
                (s3', aw) <- stepWire afterWire dt (Right (output, s2))
                whenRight s3' $ \ s3 -> run s3 session' w bw aw

    mainWire :: GameWire impl () (PureOutput impl)
    mainWire = proc _ -> do
        rec
            (world, output) <- gameWire . force . delay simpleWorld -< world
        returnA -< output

    whenRight :: Either a b -> (b -> IO ()) -> IO ()
    whenRight = flip (either (const (return ())))
