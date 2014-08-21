{-# LANGUAGE Rank2Types, FlexibleContexts, GADTs, Arrows #-}
module GLUtils where 

import Prelude hiding (init)
import System.IO
import Control.Monad
import Data.Vector.Storable (unsafeWith)
import Graphics.Rendering.OpenGL hiding (RenderMode)
import Graphics.UI.GLFW
import Data.Char
import qualified Codec.Picture as JP
import Data.IORef
import qualified Data.Set as S
import qualified GLConfig as A
import Control.Lens

------ datatypes ------
type GLpoint2D = (GLfloat, GLfloat)

type Keys = S.Set Key

data RenderMode = NormalRender
data AtlasSize = AtlasSize Double Double
data Resolution = Resolution Double Double

type RenderFunc = Resolution -> AtlasSize -> RenderMode -> IO ()

type RenderQueue = [RenderFunc]

vertex3 :: (Real a, Fractional a) => a -> a -> a -> Vertex3 GLfloat
vertex3 x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

texCoord2 :: (Real a, Fractional a) => a -> a -> TexCoord2 GLfloat
texCoord2 x y = TexCoord2 (realToFrac x) (realToFrac y)
 
color3 :: GLfloat -> GLfloat -> GLfloat -> Color3 GLfloat
color3 = Color3

white, brown, green, red, blue, gray, yellow :: Color3 GLfloat
white = color3 1 1 1
brown = color3 0.4 0.2 0.1
green = color3 0 0.5 0
red = color3 0.5 0 0
blue = color3 0 0 0.5
gray = color3 0.5 0.5 0.5
yellow = color3 1 1 0

data Button = BKey Key | CKey Char
data DrawMode = BigMenuMode | GameMode

type Menu = ([String],Int)
------ menu ------
defaultMenu :: Menu
defaultMenu = ([
    "Tab: game menu",
    "Enter: write command",
    "g: restart",
    "p: pause",
    "arrows: move focus",
    "</>: up/down"
    ],-1)

gameMenu :: Menu
gameMenu =  ([
    "Return",
    "Quit"
    ],0)

    
moveSel :: Menu -> Int -> Menu
moveSel (m,-1) _ = (m,-1)
moveSel (m,s) x = (m, s+x`mod` length m)
    
------ callbacks --------
errorCallback :: ErrorCallback
errorCallback _ = hPutStrLn stderr 

windowSizeCallback :: Window -> Int -> Int -> IO ()
windowSizeCallback _ = prepareViewport

setInput :: Window -> IORef Keys -> IO ()
setInput win keysRef = setKeyCallback win (Just keyCallback)
  where
    keyCallback _ k _ KeyState'Pressed _ = modifyIORef' keysRef (S.insert k)
    keyCallback _ k _ KeyState'Released _ = modifyIORef' keysRef (S.delete k)
    keyCallback _ _ _ _ _= return ()

------ Ui and Window preparation ------
setupUi :: IO (Window, TextureObject)
setupUi = do
    setErrorCallback (Just errorCallback)
    _ <- init
    -- open window
    Just win <- createWindow 800 600 "neflEFortress" Nothing Nothing    -- [DisplayAlphaBits 8] Window
    makeContextCurrent (Just win)
    -- set the color to clear background
    clearColor $= Color4 0 0 0 0
    -- load texture
    tex <- loadTexture "tileset.png"
    texture Texture2D $= Enabled
    prepareViewport 800 600

    clearColor $= Color4 0.1 0.6 0.8 0
    
    frontFace $= CCW
    cullFace $= Just Back
    -- enable transparency
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    --alphaFunc $= Just (Greater, 0.1)

    return (win, tex)

prepareViewport:: Int -> Int -> IO ()
prepareViewport w h = do
    viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (realToFrac w) 0 (realToFrac h)

------ textures ------
loadTexture :: String -> IO TextureObject
loadTexture imagePath = do
    textureName <- genObjectName 
    textureBinding Texture2D $= Just textureName
    textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)
    image <- JP.readImage imagePath
    case image of 
        (Left s) -> print s
        (Right (JP.ImageRGBA8 (JP.Image width height dat))) -> 
            -- Access the data vector pointer
            unsafeWith dat $ \ptr ->
                texImage2D Texture2D NoProxy 0 RGBA8 
                    (TextureSize2D (toEnum width) (toEnum height))
                    0 (PixelData RGBA UnsignedByte ptr)
        (Right _) -> print "image not found"
    return textureName
            
------ drawing ------
{-drawMenu :: Menu -> GLpoint2D -> GLpoint2D -> IO()
drawMenu ([],_) _ _ = return()
drawMenu (x : xs, i) (destX, destY) (w, h) = do
    if i == 0
        then do 
            color $ color3 0.5 1 0.5
            drawString ('@':x) (destX, destY) (w,h)
            color $ color3 1 1 1
        else drawString (' ':x) (destX, destY) (w,h)
    drawMenu (xs,i-1) (destX, destY+h) (w,h)-}

drawImage :: Double -> Double -> A.Atlas -> RenderFunc 
drawImage x y a (Resolution resX resY) (AtlasSize aW aH) mode = do 
    let (aX, aY) = A.atlas a 
        (aSX, aEX) = ((aX - 1) / aW, aX / aW)
        (aSY, aEY) = ((aY - 1) / aH, aY / aH)
        (sX, eX) = (x * resX, (x + 1) * resX)
        (sY, eY) = (y * resY, (y + 1) * resY)
    texCoord $ texCoord2  aSX aSY 
    vertex   $ vertex3     sX  eY 0
    texCoord $ texCoord2  aSX aEY 
    vertex   $ vertex3     sX  sY 0
    texCoord $ texCoord2  aEX aEY 
    vertex   $ vertex3     eX  sY 0
    texCoord $ texCoord2  aEX aSY 
    vertex   $ vertex3     eX  eY 0

drawColored :: Double -> Double -> A.Atlas -> Color3 GLfloat-> RenderFunc
drawColored x y a col r aS m = do
    color col
    drawImage x y a r aS m
    color white

{-drawString :: String -> GLpoint2D -> GLpoint2D  -> RenderFunc
drawString [] _ _  mode = return()
drawString (c:cs) (destX, destY) (w, h) mode = do
    drawGeneric (charAtlas c) (destX, destY) (w, h) mode
    drawString cs (destX + w, destY) (w, h) mode-}
    
