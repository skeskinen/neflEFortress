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
    coQuit          = coQuit'
    coMoveCursorRel = coMoveCursorRel'
    coStartSelect   = coStartSelect'
    coCancelSelect  = coCancelSelect'
    coDig           = coDig'
                    

type GLWire a b = GameWire GLUi a b

--Drawing, maybe other stuff later
processWorld' :: GLWire UiWorld GLOutput
processWorld' = arr (\ UiWorld{tiles = t, creatures = c, buildings = b, items = i 
                              ,focusPos = f, selectPos = sel} -> 
        GLOutput (mkTiles t ++ mkFocus f))
  where 
      --draw creatures, buildings, items here
    mkTiles = foldWithIndices drawTile [] 
    mkFocus (x,y,_) = [drawImage (fromIntegral x) (fromIntegral y) A.Focus]

--Commands
coQuit' :: GLWire a (Event a)
coQuit' = keyDown Key'Q

coMoveCursorRel' :: GLWire a (Event (Dir, Int))
coMoveCursorRel' = mRight <& mLeft <& mUp <& mDown <& mTop <& mBottom
  where
    mkDir k d = keyDown k . mkConst (Right (d, 1))
    mRight  = mkDir Key'Right  DRight
    mLeft   = mkDir Key'Left   DLeft
    mUp     = mkDir Key'Up     DUp
    mDown   = mkDir Key'Down   DDown
    mTop    = mkDir Key'Period DTop
    mBottom = mkDir Key'Comma  DBottom

coDig' :: GLWire a (Event a)
coDig' = keyDown Key'D

coStartSelect' :: GLWire a (Event a)
coStartSelect' = keyDown Key'Space

coCancelSelect' :: GLWire a (Event a)
coCancelSelect' = keyDown Key'Escape

--fps stuff
outputFreq' :: GLUi -> MWire IO a (Event a)
outputFreq' _ = periodic 0.02

worldFreq' :: GLWire a (Event a)
worldFreq' = periodic 0.06

--IO stuff
newGLUi :: IO ()
newGLUi = setupUi GLUi >>= runUiImpl

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

sleep' :: GLUi -> IO ()
sleep' _ = threadDelay 10000 

--Keyboard utils, needs to see GLInput
keyDown :: Key -> GLWire a (Event a)
keyDown k = off
  where 
    pressed = asks (S.member k . keys)
    off = mkGenN $ \ a -> pressed >>= (\p -> if p then e a else ne off)
    on = mkGenN $ \ _ -> pressed >>= (\p -> ne (if p then on else off))
    e a = return (Right (Event a), on)
    ne w = return (Right NoEvent, w)

keyPressed :: Key -> GLWire a a
keyPressed k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Right a else Left mempty)

keyNotPressed :: Key -> GLWire a a
keyNotPressed k = mkGen_ $ \ a -> do
    pressed <- asks (S.member k . keys)
    return (if pressed then Left mempty else Right a)

