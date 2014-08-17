{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts, GADTs, Arrows #-}
module Ui where

import Control.Monad.Reader
import Control.Monad
import Control.Lens

import Prelude hiding ((.), id, until)
import Control.Wire
import FRP.Netwire

import Utils
import World
import Terrain
import WorldGenerating

type GameWire r a b = Wire (Timed NominalDiffTime ()) () (Reader r) a b
type IOWire a b = Wire (Timed NominalDiffTime ()) () IO a b

data UiHole i o s a where
    Init :: UiHole i o s (IO s)
    BeforeFrame :: UiHole i o s (IOWire s (i,s))
    AfterFrame :: UiHole i o s (IOWire (o,s) s)
    MakeOutput :: UiHole i o s (GameWire i () o)

newtype UiImpl i o s = UiImpl { runImpl :: forall a. UiHole i o s a -> a }

runUiImpl :: UiImpl i o s -> IO () 
runUiImpl impl = do
  s0 <- runImpl impl Init
  let bw = runImpl impl BeforeFrame
      aw = runImpl impl AfterFrame
  run s0 clockSession_ (mainWire impl) bw aw
 where
  run s1 session wire beforeWire afterWire = do
    (dt, session') <- stepSession session
    (s2', bw) <- stepWire beforeWire dt (Right s1)
    right s2' $ \ (input, s2) -> do
      let (output', w) = runReader (stepWire wire dt (Right undefined)) input
      right output' $ \ output -> do
        (s3', aw) <- stepWire afterWire dt (Right (output, s2))
        right s3' $ \ s3 -> run s3 session' w bw aw

  mainWire :: UiImpl i o s -> GameWire i World' o
  mainWire impl = proc _ -> do
    runImpl impl MakeOutput -< ()

  right :: Either a b -> (b -> IO ()) -> IO ()
  right e = flip (either (const (return ()))) e
    


