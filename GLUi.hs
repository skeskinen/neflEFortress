{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts, GADTs #-}
module GLUi (newGLUi) where

import Control.Lens
import Control.Monad
import Data.Monoid
import Control.Monad.Reader
import Control.Applicative
import Data.IORef
import qualified Data.Set as S

import Graphics.UI.GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit

import Prelude hiding ((.), id, until)
import Control.Wire
import FRP.Netwire

import Ui
import UiUtils
import GLUtils
import World
import Terrain

data RenderMode = NormalRender
type RenderFunc = RenderMode -> IO ()
newtype RenderQueue = RenderQueue [RenderFunc]

data UiState = UiState { win :: Window, keysRef :: IORef Keys }
data Output = Output { rque :: RenderQueue }
data Input = Input { keys :: Keys }
type Keys = S.Set Key

type GLWire a b = GameWire Input a b


impl :: UiHole Input Output UiState a -> a
impl Init = uiInit 
impl BeforeFrame = getInput
impl AfterFrame = draw
impl MakeOutput = arr (\ _ -> Output $ RenderQueue [])

newGLUi :: IO ()
newGLUi = runUiImpl $ UiImpl impl
    
uiInit :: IO UiState
uiInit = do
    win <- setupUi
    setWindowSizeCallback win (Just windowSizeCallback)
    keysRef <- newIORef S.empty
    setInput win keysRef
    return $ UiState win keysRef

draw :: IOWire (Output, UiState) UiState
draw = mkGen_ $ \ (Output{rque = r}, s@UiState{win = w}) -> do
    return (Right s)
    {-
    GL.clear [GL.ColorBuffer]
    GL.renderPrimitive GL.Quads $ 
        zipWithM_ (flip ($)) (repeat NormalRender) rque
    swapBuffers win
    -}

setInput :: Window -> IORef Keys -> IO ()
setInput win keysRef = setKeyCallback win (Just keyCallback)
  where
    keyCallback _ k _ KeyState'Pressed _ = modifyIORef' keysRef (S.insert k)
    keyCallback _ k _ KeyState'Released _ = modifyIORef' keysRef (S.delete k)
    keyCallback _ _ _ _ _= return ()

getInput :: IOWire UiState (Input, UiState) 
getInput = mkGen_ $ \ (s@UiState{keysRef = keysRef}) -> do
    pollEvents
    k <- readIORef keysRef
    return (Right (Input k, s))

keyDown :: Key -> GLWire a a
keyDown k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Right a else Left mempty)

keyUp :: Key -> GLWire a a
keyUp k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Left mempty else Right a)


