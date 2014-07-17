{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module GLUI (newGLUI) where

import Control.Applicative
import Data.Maybe
import Control.Lens
import Control.Lens.Zoom
import System.Environment
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split (chunksOf)
import Data.Vector.Lens
import qualified Data.Map as M
import qualified Data.IntSet as IS

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef

import UI
import UIUtils
import GLUtils
import World
import Terrain

type GLUI = StateT GLUIState IO



data GLUIState = GLUIState {
    _glUIState :: UIState,
    _glCommandHandlers :: M.Map String (GLUI ()),
    _glLastStep :: Double,
    _glLastRender :: Double
}

simpleGLUIState :: GLUIState
simpleGLUIState = GLUIState {
    _glUIState = simpleUIState,
    _glCommandHandlers = M.empty,
    _glLastStep = -1,
    _glLastRender = -1
}

makeLenses ''GLUIState

newGLUI :: IO ()
newGLUI = void (runStateT start simpleGLUIState)

start :: GLUI ()
start = do
    
    liftIO setupUI
    setCallbacks
    until_ (use (glUIState . uiQuit)) loop
    liftIO GLFW.closeWindow
    liftIO GLFW.terminate

    
setCallbacks :: GLUI ()    
setCallbacks = do 
    -- set 2D orthogonal view inside windowSizeCallback because
    -- any change to the Window size should result in different
    -- OpenGL Viewport.
    liftIO $ GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection -- ???
        GL.loadIdentity 
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
   
    


loop :: GLUI ()
loop = do
    t0 <- liftIO (GL.get GLFW.time)
    ls <- use glLastStep
    lr <- use glLastRender
    handleInput
    when ((t0-ls)>=0.5) $  do
        glLastStep .= t0
        zoomUI run
    when ((t0-lr)>=0.1) $ do
        glLastRender .= t0
        draw
    liftIO $ GLFW.sleep (0.01)

drawTileArray :: [[Tile]] -> IO ()
drawTileArray tiles = unwindWithIndicesM_ tiles drawTile

drawTile :: Tile -> Int -> Int -> IO ()
drawTile tile x y = do
    -- terrain
    case (tile ^. tileType) of 
        TileGround -> drawIm "ground"
        TileWall _ -> drawIm "wall"
        TileStairs -> drawIm "stairs"
        TileEmpty -> drawIm "empty"
        otherwise -> drawIm "empty"
    -- building
    when ((not . IS.null) $ tile ^. tileBuildings) $ drawIm "building"
    -- item
    when ((not . IS.null) $ tile ^. tileItems) $ drawIm "item"
    -- creature
    when ((not . IS.null) $ tile ^. tileCreatures) $ drawIm "creature"
    
    where drawIm = drawImage point1 point2
          point1 = (fromIntegral $ 32*x, fromIntegral $ 32*y)::GLpoint2D
          point2 = (fromIntegral $ 32*(x+1), fromIntegral $ 32*(y+1))::GLpoint2D

draw :: GLUI ()
draw = do
    liftIO $ GL.clear [GL.ColorBuffer]
    t <- use $ glUIState . uiWorld . worldTerrain
    f <- use $ glUIState . uiCamera . _3
    let floor = getFloor t f
    let w = floor ^. terrainWidth
    liftIO $ GL.renderPrimitive GL.Quads $  
        drawTileArray $ chunksOf w $ floor  ^. (terrainTiles . from vector)
    liftIO GLFW.swapBuffers


    
 
handleInput :: GLUI ()
handleInput = do
    p <- liftIO $ GLFW.getKey GLFW.ESC
    windowOpen <- liftIO $ getParam Opened
    when ((p == GLFW.Press) || (not windowOpen)) $ glUIState . uiQuit .= True
    



zoomUI :: UI a -> GLUI a
zoomUI = zoom glUIState

 
 
 