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
import Text.ParserCombinators.Parsec

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM

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
    _glLastRender :: Double,
    _glWindow :: GLFW.Window,
    _glCharQueue :: TQueue Char,
    _glKeyQueue :: TQueue GLFW.Key
}

simpleGLUIState :: IO GLUIState
simpleGLUIState = do
    window <- liftIO setupUI
    cQue <- liftIO $ newTQueueIO :: IO (TQueue Char)
    kQue <- liftIO $ newTQueueIO :: IO (TQueue GLFW.Key)
    return GLUIState {
        _glUIState = simpleUIState,
        _glCommandHandlers = M.empty,
        _glLastStep = -1,
        _glLastRender = -1,
        _glWindow = window,
        _glCharQueue = cQue,
        _glKeyQueue = kQue
    }

makeLenses ''GLUIState

newGLUI :: IO ()
newGLUI = void (simpleGLUIState >>= (\s -> runStateT start s))

start :: GLUI ()
start = do
    win <- use glWindow
    setCallbacks win
    until_ (use (glUIState . uiQuit)) loop
    liftIO $ GLFW.destroyWindow win
    liftIO $ GLFW.terminate
 
setCallbacks :: GLFW.Window -> GLUI ()    
setCallbacks win = do 
    cQue <- use glCharQueue
    kQue <- use glKeyQueue
    liftIO $ GLFW.setWindowSizeCallback win (Just windowSizeCallback)
    liftIO $ GLFW.setCharCallback win (Just (charCallback cQue))
    liftIO $ GLFW.setKeyCallback win (Just (keyCallback kQue))
    return ()
 
loop :: GLUI ()
loop = do
    win <- use glWindow
    Just t0 <- liftIO GLFW.getTime
    ls <- use glLastStep
    lr <- use glLastRender
    handleInput win
    when ((t0-ls)>=0.5) $  do
        glLastStep .= t0
        zoomUI run
    when ((t0-lr)>=0.1) $ do
        glLastRender .= t0
        draw win
    liftIO $ threadDelay 100000 

drawTileArray :: [[Tile]] -> IO ()
drawTileArray tiles = unwindWithIndicesM_ tiles drawTile

drawTile :: Tile -> Int -> Int -> IO ()
drawTile tile x y = do
    case (tile ^. tileType) of 
        TileGround -> drawIm "ground"
        TileWall _ -> drawIm "wall"
        TileStairs -> drawIm "stairs"
        TileEmpty -> drawIm "empty"
        otherwise -> drawIm "empty"
    when ((not . IS.null) $ tile ^. tileBuildings) $ drawIm "building"
    when ((not . IS.null) $ tile ^. tileItems) $ drawIm "item"
    when ((not . IS.null) $ tile ^. tileCreatures) $ drawIm "creature" 
    where drawIm = drawImage point1 point2
          point1 = (fromIntegral $ 32*x, fromIntegral $ 32*y)::GLpoint2D
          point2 = (fromIntegral $ 32*(x+1), fromIntegral $ 32*(y+1))::GLpoint2D

draw :: GLFW.Window -> GLUI ()
draw win= do
    liftIO $ GL.clear [GL.ColorBuffer]
    t <- use $ glUIState . uiWorld . worldTerrain
    f <- use $ glUIState . uiCamera . _3
    x <- use $ glUIState . uiCamera . _1
    y <- use $ glUIState . uiCamera . _2
    
    let floor = getFloor t f
    let w = floor ^. terrainWidth
    let p1 = (fromIntegral $ 32*x, fromIntegral $ 32*y) 
    let p2 = (fromIntegral $ 32*(x+1), fromIntegral $ 32*(y+1))
    liftIO $ GL.renderPrimitive GL.Quads $  do
        -- tiles
        drawTileArray $ chunksOf w $ floor  ^. (terrainTiles . from vector)
        -- focus
        drawImage p1 p2 "focus"
        drawString (352, 32) (8,16) ("x: " ++ (show x))
        drawString (352, 48) (8,16) ("y: " ++ (show y))
        drawString (352, 64) (8,16) ("z: " ++ (show f))
    
    
    liftIO $ GLFW.swapBuffers win

handleInput :: GLFW.Window -> GLUI ()
handleInput win = do
    liftIO $ GLFW.pollEvents
    handleChars
    handleKeys
    p <- liftIO $ GLFW.getKey win GLFW.Key'Escape
    c <- liftIO $ GLFW.windowShouldClose win
    when c $ glUIState . uiQuit .= True

handleChars :: GLUI ()
handleChars = do
    -- reading from the channel:
    que <- use glCharQueue
    emptyQueue <- liftIO $ atomically $ isEmptyTQueue que
    when (not emptyQueue) $ do
        c <- liftIO $ atomically $ readTQueue que
        case c of
            '>' -> execString "descendCamera"
            '<' -> execString "ascendCamera"
            'g' -> execString "genWorld"
            'p' -> execString "pause"
            _ -> return()
        handleChars

handleKeys :: GLUI ()
handleKeys = do
    -- reading from the channel:
    que <- use glKeyQueue
    emptyQueue <- liftIO $ atomically $ isEmptyTQueue que
    when (not emptyQueue) $ do
        k <- liftIO $ atomically $ readTQueue que
        case k of
            GLFW.Key'Backspace -> execString "pause"
            GLFW.Key'Escape -> execString "quit" --hits twice/press
            GLFW.Key'Enter -> execString "pause"
            GLFW.Key'Up -> (glUIState . uiCamera . _2) -= 1 
            GLFW.Key'Down -> (glUIState . uiCamera . _2) += 1
            GLFW.Key'Right -> (glUIState . uiCamera . _1) += 1 
            GLFW.Key'Left -> (glUIState . uiCamera . _1) -= 1
            _ -> return()
        handleChars


execString :: String -> GLUI ()
execString str = do
    r <- return(parseCommand str)
    case r of
            Left _ -> return ()
            Right (cmd, arg) -> do
                zoomUI $ execCommand cmd arg

zoomUI :: UI a -> GLUI a
zoomUI = zoom glUIState

    
 
 