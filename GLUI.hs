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
    _glKeyQueue :: TQueue GLFW.Key,
    _glResolution :: GLpoint2D,
    _glMenu :: Menu
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
        _glKeyQueue = kQue,
        _glResolution = (32,32),
        _glMenu = gameMenu
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
    Just t0 <- liftIO GLFW.getTime
    ls <- use glLastStep
    lr <- use glLastRender
    handleInput
    when ((t0-ls)>=0.1) $  do
        glLastStep .= t0
        zoomUI run
    when ((t0-lr)>=0.05) $ do
        glLastRender .= t0
        draw
    liftIO $ threadDelay 100000 

drawTileArray :: GLpoint2D -> (Int,Int) -> (Int,Int) -> [[Tile]] -> IO ()
drawTileArray res mins maxs tiles = unwindWithIndicesM_ tiles (drawTile res mins maxs)

drawTile :: GLpoint2D -> (Int,Int) -> (Int,Int) -> Tile -> Int -> Int -> IO ()
drawTile (rw,rh) (minX,minY) (maxX, maxY) tile x y =
    when ((x>=minX)&&(x<=maxX)&&(y>=minY)&&(y<=maxY)) $ do
        case (tile ^. tileType) of 
            TileGround -> drawIm "ground"
            TileWall _ -> drawIm "wall"
            TileStairs -> drawIm "stairs"
            TileEmpty -> drawIm "empty"
            otherwise -> drawIm "empty"
        when ((not . IS.null) $ tile ^. tileBuildings) $ drawIm "building"
        when ((not . IS.null) $ tile ^. tileItems) $ drawIm "item"
        when ((not . IS.null) $ tile ^. tileCreatures) $ do
            GL.color $ color3 0 1 0
            drawIm "creature2"
            GL.color $ color3 1 1 1
        where drawIm = \im -> drawImage im point1 (rw,rh)
              point1 = (rw*(fromIntegral x), rh*(fromIntegral y))

draw :: GLUI ()
draw = do
    liftIO $ GL.clear [GL.ColorBuffer]
    t <- use $ glUIState . uiWorld . worldTerrain
    f <- use $ glUIState . uiCamera . _3
    x <- use $ glUIState . uiCamera . _1
    y <- use $ glUIState . uiCamera . _2
    (rw,rh) <- use $ glResolution
    
    win <- use glWindow
    (winW, winH) <- liftIO $ GLFW.getWindowSize win
    let (bx,by) = getBorders (winW, winH) (rw, rh)
    
    let floor = getFloor t f
    let w = floor ^. terrainWidth
    let h = floor ^. terrainHeight
    let focusPoint = (rw*(fromIntegral x), rh*(fromIntegral y))
    menu <- use glMenu
    liftIO $ GL.renderPrimitive GL.Quads $  do
        -- tiles
        drawTileArray (rw, rh) (0,0) (bx-1,by-1) $ chunksOf w $ floor  ^. (terrainTiles . from vector)
        -- focus
        drawImage "focus" focusPoint (rw,rh)
        -- coordinates
        let drawCoor = \string xPos -> drawString string  (rw*xPos, rh*(fromIntegral (min h by)))   (rw/2,rh)
        drawCoor ("x: " ++ show x) 1    
        drawCoor ("y: " ++ show y) 4
        drawCoor ("z: " ++ show f) 7
        drawCoor ("w: " ++ show winW) 10
        drawCoor ("h: " ++ show winH) 15
        -- menu
        drawMenu menu (rw*(fromIntegral (min bx w)),rh) (rw/2,rh)
    liftIO $ GLFW.swapBuffers win

handleInput :: GLUI ()
handleInput = do
    win <- use glWindow
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
        men <- use glMenu
        let m = moveSel men 1
        case c of
            '>' -> execString "descendCamera"
            '<' -> execString "ascendCamera"
            'g' -> execString "genWorld"
            'p' -> execString "pause"
            '+' -> glMenu .= moveSel men 1
            '-' -> glMenu .= moveSel men (-1)
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

getBorders :: (Int,Int) -> GLpoint2D -> (Int,Int)
getBorders (winW,winH) (rw,rh) =
    ((+(-10)) . floor $ ((fromIntegral winW)/rw),
     (+(-1)). floor $ ((fromIntegral winH)/rh))
 
 