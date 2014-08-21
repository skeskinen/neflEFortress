{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, Arrows, 
        ScopedTypeVariables #-}
module Ui where

import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad
--import Control.Lens

import Prelude hiding ((.), id, until)
import Control.Wire
--import FRP.Netwire

--import Utils
import World
--import Terrain
import WorldGenerating

type GameWire r a b = Wire (Timed NominalDiffTime ()) () (Reader r) a b
type IOWire a b = Wire (Timed NominalDiffTime ()) () IO a b

data UiHole i o s a where 
    Start :: UiHole i o s (IO s)
    BeforeFrame :: UiHole i o s (IOWire s (i,s))
    AfterFrame :: UiHole i o s(IOWire (o,s) s)
    GameWire :: UiHole i o s (GameWire i World (World, o))

type UiImpl i o s = forall a. (UiHole i o s a) -> a

runUiImpl :: forall i o s. UiImpl i o s -> IO () 
runUiImpl impl = do
    s0 <- impl Start
    let bw = impl BeforeFrame 
        aw = impl AfterFrame
    run s0 clockSession_ mainWire bw aw
  where
    run s1 session wire beforeWire afterWire = do
        (dt, session') <- stepSession session
        (s2', bw) <- stepWire beforeWire dt (Right s1)
        whenRight s2' $ \ (input, s2) -> do
            let (output', w) = runReader (stepWire wire dt (Right () )) input
            whenRight output' $ \ output -> do
                (s3', aw) <- stepWire afterWire dt (Right (output, s2))
                whenRight s3' $ \ s3 -> run s3 session' w bw aw

    mainWire :: GameWire i () o
    mainWire = proc _ -> do
        rec
            (world, output) <- impl GameWire . force . delay simpleWorld -< world
        returnA -< output

    whenRight :: Either a b -> (b -> IO ()) -> IO ()
    whenRight = flip (either (const (return ())))

