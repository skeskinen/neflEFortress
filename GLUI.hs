{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module GLUI (newGLUI) where

import Data.Maybe
import Control.Lens
import Control.Lens.Zoom
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.List.Split (chunksOf)
import Data.Vector.Lens
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Text.ParserCombinators.Parsec

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
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
    _glKeyQueue :: TQueue Button,
    _glResolution :: GLpoint2D,
    _glMenu :: Menu,
    _glMenuActions :: [GLUI()],
    _glBindings :: Button -> GLUI(),
    _glDrawMode :: DrawMode,
    _glWriteInput :: (Bool, String)
}

simpleGLUIState :: IO GLUIState
simpleGLUIState = do
    window <- liftIO setupUI
    kQue <- liftIO $ newTQueueIO :: IO (TQueue Button)
    return GLUIState {
        _glUIState = simpleUIState,
        _glCommandHandlers = M.empty,
        _glLastStep = -1,
        _glLastRender = -1,
        _glWindow = window,
        _glKeyQueue = kQue,
        _glResolution = (32,32),
        _glMenu = defaultMenu,
        _glMenuActions = [],
        _glBindings = \k -> return(),
        _glDrawMode = GameMode,
        _glWriteInput = (False,"")
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

-- sets various callbacks and bindings    
setCallbacks :: GLFW.Window -> GLUI ()    
setCallbacks win = do
    kQue <- use glKeyQueue
    liftIO $ GLFW.setWindowSizeCallback win (Just windowSizeCallback)
    liftIO $ GLFW.setCharCallback win (Just (charCallback kQue))
    liftIO $ GLFW.setKeyCallback win (Just (keyCallback kQue))
    glBindings .= defaultBindings
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

------ drawing ------
draw :: GLUI ()
draw = do
    mode <- use glDrawMode
    case mode of 
        GameMode -> gameRender
        BigMenuMode -> bigMenuRender

gameRender :: GLUI()
gameRender = do
    liftIO $ GL.clear [GL.ColorBuffer]
    t <- use $ glUIState . uiWorld . worldTerrain
    f <- use $ glUIState . uiCamera . _3
    x <- use $ glUIState . uiCamera . _1
    y <- use $ glUIState . uiCamera . _2
    
    (rw,rh) <- use $ glResolution
    win <- use glWindow
    (winW, winH) <- liftIO $ GLFW.getWindowSize win
    let (bx,by) = getBorders (winW, winH) (rw, rh)
    
    (writing,text) <- use glWriteInput
    
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
        -- bottom
        let drawBot = \string xPos -> drawString string  (rw*xPos, rh*(fromIntegral (min h by)))   (rw/2,rh)
        if writing 
            then drawBot ("command: " ++ text) 1 
            else do
                drawBot ("x: " ++ show x) 1    
                drawBot ("y: " ++ show y) 4
                drawBot ("z: " ++ show f) 7
                drawBot ("w: " ++ show winW) 10
                drawBot ("h: " ++ show winH) 15
        -- menu
        drawMenu menu (rw*(fromIntegral (min bx w)),rh) (rw/2,rh)
    liftIO $ GLFW.swapBuffers win

bigMenuRender :: GLUI()
bigMenuRender = do
    liftIO $ GL.clear [GL.ColorBuffer]
    (rw,rh) <- use $ glResolution
    win <- use glWindow
    (winW, winH) <- liftIO $ GLFW.getWindowSize win
    menu <- use glMenu
    liftIO $ GL.renderPrimitive GL.Quads $  do
        drawMenu menu ((fromIntegral winW)/3,rh*2) (rw/2,rh)
    liftIO $ GLFW.swapBuffers win
    
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

------ input handling ------
handleInput :: GLUI ()
handleInput = do
    win <- use glWindow
    liftIO $ GLFW.pollEvents
    handleKeys
    c <- liftIO $ GLFW.windowShouldClose win
    when c $ glUIState . uiQuit .= True

handleKeys :: GLUI ()
handleKeys = do
    que <- use glKeyQueue
    emptyQueue <- liftIO $ atomically $ isEmptyTQueue que
    when (not emptyQueue) $ do
        k <- liftIO $ atomically $ readTQueue que
        handler <- use glBindings
        handler k
        handleKeys

defaultBindings ::  Button -> GLUI ()
defaultBindings k =do
    men <- use glMenu
    (writing,text) <- use glWriteInput
    case k of
        BKey b -> case b of
            GLFW.Key'Escape -> if writing 
                                  then endWriting
                                  else execString "quit"
            GLFW.Key'Tab -> toGameMenu
            GLFW.Key'Enter -> do when writing (execString text)
                                 startWriting
            GLFW.Key'Backspace -> if writing then removeChar else return()
            GLFW.Key'Up -> moveFocus 0 (-1) 0
            GLFW.Key'Down -> moveFocus 0 1 0
            GLFW.Key'Right -> moveFocus 1 0 0 
            GLFW.Key'Left -> moveFocus (-1) 0 0
            GLFW.Key'Pad1 -> moveFocus (-1) 1 0
            GLFW.Key'Pad2 -> moveFocus 0 1 0
            GLFW.Key'Pad3 -> moveFocus 1 1 0
            GLFW.Key'Pad4 -> moveFocus (-1) 0 0
            GLFW.Key'Pad5 -> execString "pause"
            GLFW.Key'Pad6 -> moveFocus 1 0 0
            GLFW.Key'Pad7 -> moveFocus (-1) (-1) 0
            GLFW.Key'Pad8 -> moveFocus 0 (-1) 0
            GLFW.Key'Pad9 -> moveFocus 1 (-1) 0
            _ -> return()
        CKey c -> do if (writing) 
                        then writeChar c
                        else case c of
                                '>' -> moveFocus 0 0 1
                                '<' -> moveFocus 0 0 (-1)
                                'g' -> execString "genWorld"
                                'p' -> execString "pause"
                                '+' -> glMenu .= moveSel men 1
                                '-' -> glMenu .= moveSel men (-1)
                                _ -> return()

gmBindings ::  Button -> GLUI ()
gmBindings k = case k of
    BKey b -> case b of
        GLFW.Key'Enter -> selectMenu
        GLFW.Key'Escape -> execString "quit"
        GLFW.Key'Tab -> toDefaultMenu
        GLFW.Key'Up -> do 
            men <- use glMenu
            glMenu .= moveSel men 1
        GLFW.Key'Down -> do
            men <- use glMenu
            glMenu .= moveSel men (-1)
        _ -> return()
    CKey c -> case c of
        '+' -> do men <- use glMenu
                  glMenu .= moveSel men 1
        '-' -> do men <- use glMenu
                  glMenu .= moveSel men (-1)
        _ -> return()

------ writing ------
writeChar :: Char -> GLUI()
writeChar c = do
    (b, s) <- use glWriteInput
    glWriteInput .= (b,s++[c])

removeChar :: GLUI()
removeChar = do
    (b, s) <- use glWriteInput
    when (not (null s)) $ glWriteInput .= (b,init s)
    
endWriting :: GLUI()
endWriting = glWriteInput .= (False,"")

startWriting :: GLUI()
startWriting =  glWriteInput .= (True,"")

------ menus ------
toGameMenu :: GLUI()
toGameMenu = do
    glMenu .= gameMenu
    glBindings .= gmBindings
    glMenuActions .= gmActions
    glDrawMode .= BigMenuMode

gmActions :: [GLUI()]
gmActions = [toDefaultMenu, execString "quit"]

toDefaultMenu :: GLUI()
toDefaultMenu = do
    glMenu .= defaultMenu
    glBindings .= defaultBindings
    glMenuActions .= []
    glDrawMode .= GameMode

selectMenu :: GLUI()
selectMenu = do
    act <- use glMenuActions
    (_,i) <- use glMenu
    if (length act) <= i
        then return()
        else act !! i

------ stuff ------
moveFocus :: Int -> Int -> Int -> GLUI()
moveFocus dx dy dz = do
    mx <- use $ glUIState . uiWorld . worldTerrain . terrainWidth
    my <- use $ glUIState . uiWorld . worldTerrain . terrainHeight
    mz <- use $ glUIState . uiWorld . worldTerrain . terrainDepth
    x <- use $ glUIState . uiCamera . _1
    y <- use $ glUIState . uiCamera . _2
    z <- use $ glUIState . uiCamera . _3
    glUIState . uiCamera . _1 .= (x+dx)`mod`mx
    glUIState . uiCamera . _2 .= (y+dy)`mod`my
    glUIState . uiCamera . _3 .= (z+dz)`mod`mz

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
 
 