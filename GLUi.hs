{-# LANGUAGE Arrows, TypeFamilies #-}
module GLUi (newGLUi) where

import Control.Lens
import Control.Monad
--import Data.Monoid
import Control.Monad.Reader
import Control.Applicative
import Data.IORef
import qualified Data.Set as S
import Control.Monad.State
import qualified Data.IntSet as IS

import Graphics.UI.GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Prelude hiding ((.), id, until)
import Control.Wire
import FRP.Netwire

import Ui
import UiUtils
import GLUtils
import World
import Terrain
import qualified GLConfig as A

import qualified Graphics.Rendering.FTGL as Font

data GLUi = GLUi { win :: Window, keysRef :: IORef Keys 
                 , atlas :: GL.TextureObject, font :: Font.Font }

instance UiImpl GLUi where
    data PureInput GLUi  = GLInput { keys :: !Keys } 
    data PureOutput GLUi = GLOutput { renderQueue :: !RenderQueue }
    data GameState GLUi  = GameMode
    
    beforeFrame = getInput
    afterFrame = draw
    gameWire = glGame

type GLWire a b = GameWire GLUi a b

glGame :: GLWire World (World, PureOutput GLUi)
glGame = proc world' -> do
    world <- execOnce mkId (arr (execState stepWorld)) . second (periodic 0.1) -< (world', ()) 
    rec
        x <- integral 0 -< 1 
        y <- (deltaT (\ds a -> 2 * ds * a) . dir) + delay 11 -< y
        let pelle = drawColored x y A.Creature3 yellow

    let curFloor = getFloor 1 (world ^. worldTerrain)
        drawnTiles = foldWithIndices drawTile [] (toList curFloor) 
        focus = [drawImage 3 0 A.Focus]

    let rque = drawnTiles ++ focus ++ [pelle]
        output = GLOutput rque
    keyUp Key'Q -< (world, output)
  where
    --dir = keyDown Key'Up . (-0.0001) <|> keyDown Key'Down . 0.0001 <|> 0 
    dir = keyDown Key'Up . (-1) <|> keyDown Key'Down . 1 <|> 0 

drawTile :: Double -> Double -> Tile -> RenderQueue -> RenderQueue
drawTile x y c rque = go (c ^. tileType) ++ creatures ++ rque
  where
    f = drawImage x y
    colored = drawColored x y
    go :: TileType -> RenderQueue
    go TileGround     = [f A.Ground]
    go (TileWall _)   = [f A.Wall]
    go TileEmpty      = [f A.Empty]
    go TileStairs     = [f A.Ground, colored A.Stairs brown]
    go _              = [f A.Empty]
    creatures = if IS.null (c ^. tileCreatures) 
                    then [] else [colored A.Creature2 green]

newGLUi :: IO ()
newGLUi = uiInit >>= runUiImpl

uiInit :: IO GLUi
uiInit = do
    (glWin, glTex) <- setupUi
    setWindowSizeCallback glWin (Just windowSizeCallback)
    keysRef' <- newIORef S.empty
    setInput glWin keysRef'

    font' <- Font.createTextureFont "font.ttf"
    _ <- Font.setFontFaceSize font' 100 72
    return $ GLUi glWin keysRef' glTex font'

draw :: IOWire (PureOutput GLUi, GLUi) GLUi
draw = execOnce (arr snd) go . (mkId &&& periodic 0.02)
  where 
    go = mkGen_ $ \ (GLOutput{renderQueue = r}, s@GLUi{win = w, atlas = tex, font = f}) -> do
        GL.clear [GL.ColorBuffer]
        GL.textureBinding GL.Texture2D GL.$= Just tex
        GL.renderPrimitive GL.Quads $ 
            foldM execRenderFunc () r
        GL.preservingAttrib [GL.AllServerAttributes] $ 
            Font.renderFont f "Hello World!" Font.All
        swapBuffers w
        return (Right s)
    execRenderFunc :: () -> RenderFunc -> IO ()
    execRenderFunc _ f = f (Resolution 64 64) (AtlasSize 32 32) NormalRender

getInput :: IOWire GLUi (PureInput GLUi, GLUi) 
getInput = mkGen_ $ \ (s@GLUi{win = win', keysRef = keysRef'}) -> do
    pollEvents
    k <- readIORef keysRef'
    c <- windowShouldClose win'
    return (if c then Left () else Right (GLInput k, s))

keyDown :: Key -> GLWire a a
keyDown k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Right a else Left mempty)

keyUp :: Key -> GLWire a a
keyUp k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Left mempty else Right a)

deltaT :: Fractional a => (a -> b -> c) -> GLWire b c
deltaT f = mkSF (\ ds b -> (f (realToFrac (dtime ds)) b, deltaT f))
