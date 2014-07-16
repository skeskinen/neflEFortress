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

import Graphics.Rendering.OpenGL as GL
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
    _glCommandHandlers :: M.Map String (GLUI ())
}

simpleGLUIState :: GLUIState
simpleGLUIState = GLUIState {
    _glUIState = simpleUIState,
    _glCommandHandlers = M.empty
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
    draw
    handleInput
    zoomUI run
    liftIO $ GLFW.sleep 0.5

drawTileArray :: [[Tile]] -> IO ()
drawTileArray tiles = unwindWithIndicesM_ tiles drawTile

drawTile :: Tile -> Int -> Int -> IO ()
drawTile tile x y = do
    -- terrain
    case (tile ^. tileType) of 
        TileGround -> drawImage point1 point2 "ground"
        TileWall _ -> drawImage point1 point2 "wall"
        TileStairs -> drawImage point1 point2 "stairs"
        TileEmpty -> drawImage point1 point2 "empty"
        otherwise -> drawImage point1 point2 "empty"
    -- building
    when ((not . IS.null) $ tile ^. tileBuildings) $ drawImage point1 point2 "building"
    -- item
    when ((not . IS.null) $ tile ^. tileItems) $ drawImage point1 point2 "item"
    -- creature
    when ((not . IS.null) $ tile ^. tileCreatures) $ drawImage point1 point2 "creature"
    
    where point1 = (fromIntegral $ 32*(x-1), fromIntegral $ 32*(y-1))::GLpoint2D
          point2 = (fromIntegral $ 32*x, fromIntegral $ 32*y)::GLpoint2D

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

 
 
 