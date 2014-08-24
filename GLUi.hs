{-# LANGUAGE Arrows, TypeFamilies #-}
module GLUi (newGLUi) where

import Control.Monad
import Control.Concurrent (threadDelay)
import Data.IORef
import qualified Data.Set as S

import Graphics.UI.GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as Font

import Ui
import Prelude hiding ((.), id, until)
import GLUtils
import qualified GLConfig as A

data GLUi = GLUi { win :: Window, keysRef :: IORef Keys 
                 , atlas :: GL.TextureObject, font :: Font.Font }

type GLOutput = Output GLUi
type GLInput = Input GLUi

instance UiImpl GLUi where
    data Input GLUi  = GLInput { keys :: !Keys } 
    data Output GLUi = GLOutput { renderQueue :: !RenderQueue }
    --data GameState GLUi  = GameMode
    
    handleInput     = getInput
    handleOutput    = draw
    sleep           = sleep'
    outputFreq      = outputFreq'
    worldFreq       = worldFreq'
    processWorld    = processWorld'
    coMoveCursorRel = undefined
    coDig           = undefined
                    

type GLWire a b = GameWire GLUi a b

newGLUi :: IO ()
newGLUi = setupUi GLUi >>= runUiImpl

outputFreq' :: GLWire a (Event a)
outputFreq' = periodic 0.02

worldFreq' :: GLWire a (Event a)
worldFreq' = periodic 0.06

sleep' :: GLUi -> IO ()
sleep' _ = threadDelay 10000 

processWorld' :: GLWire UiWorld GLOutput
processWorld' = arr (\ UiWorld{tiles = t} -> 
    GLOutput (foldWithIndices drawTile [] t))

getInput :: GLUi -> IO (Maybe (GLInput, GLUi))
getInput s@GLUi{win = win', keysRef = keysRef'} = do
    pollEvents
    k <- readIORef keysRef'
    c <- windowShouldClose win'
    return (if c then Nothing else Just (GLInput k, s))

draw :: (GLOutput, GLUi) -> IO ()
draw (GLOutput{renderQueue = r}, GLUi{win = w, atlas = tex, font = font'}) = do
    GL.clear [GL.ColorBuffer]
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.renderPrimitive GL.Quads $ 
        foldM execRenderFunc () r
    GL.preservingAttrib [GL.AllServerAttributes] $ 
        Font.renderFont font' "Hello World!" Font.All
    swapBuffers w
  where
    execRenderFunc :: () -> RenderFunc -> IO ()
    execRenderFunc _ f = f (Resolution 64 64) (AtlasSize 32 32) NormalRender

keyPressed :: Key -> GLWire a a
keyPressed k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Right a else Left mempty)

keyNotPressed :: Key -> GLWire a a
keyNotPressed k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Left mempty else Right a)

--deltaT :: Fractional a => (a -> b -> c) -> GLWire b c
--deltaT f = mkSF (\ ds b -> (f (realToFrac (dtime ds)) b, deltaT f))
