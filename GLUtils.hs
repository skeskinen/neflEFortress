module GLUtils where 

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.IO
import System.Exit 
import Data.Vector.Storable (unsafeWith)
import Control.Concurrent.STM
import qualified Codec.Picture as JP

-- Datatypes
type GLpoint2D = (GLfloat, GLfloat)

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

texCoord2 :: GLfloat -> GLfloat -> GL.TexCoord2 GLfloat
texCoord2 = GL.TexCoord2
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

-- Callbacks   
errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr description

charCallback :: TChan Char -> GLFW.Window -> Char -> IO () 
charCallback chan win char = atomically $ writeTChan chan char

keyCallback :: TChan GLFW.Key -> GLFW.Window -> GLFW.Key -> IO () 
keyCallback chan win key = atomically $ writeTChan chan key

windowSizeCallback :: Window -> Int -> Int -> IO ()
windowSizeCallback win w h = prepareViewport w h

-- UI and Window preparation  
setupUI :: IO GLFW.Window
setupUI = do
    GLFW.setErrorCallback (Just errorCallback)
    GLFW.init
    -- open window
    Just win <- GLFW.createWindow 800 800 "GLFW" Nothing Nothing    -- [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.makeContextCurrent (Just win)
    -- set the color to clear background
    GL.clearColor $= Color4 0 0 0 0
    -- load texture
    loadTexture "tileset.png"
    GL.texture GL.Texture2D $= Enabled
    prepareViewport 800 800
    
    -- enable transparency
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    return win

prepareViewport:: Int -> Int -> IO ()
prepareViewport w h = do
    GL.viewport   $= (GL.Position 0 0, Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


-- Drawing and textures
drawImage :: GLpoint2D -> GLpoint2D -> String -> IO()
drawImage (destX1, destY1) (destX2, destY2) tileName = do
    let tilePos = atlas tileName
    case tilePos of 
        Just ((tileX, tileY),(atlasW, atlasH)) -> do
            GL.texCoord $ texCoord2 ((tileX-1)/atlasW) (tileY/atlasH)
            GL.vertex   $ vertex3 destX1 destY2 0
            GL.texCoord $ texCoord2 ((tileX-1)/atlasW) ((tileY-1)/atlasH)
            GL.vertex   $ vertex3 destX1 destY1 0
            GL.texCoord $ texCoord2 (tileX/atlasW) ((tileY-1)/atlasH)
            GL.vertex   $ vertex3 destX2 destY1 0
            GL.texCoord $ texCoord2 (tileX/atlasW) (tileY/atlasH)
            GL.vertex   $ vertex3 destX2 destY2 0
        Nothing -> print "bad tilename"

atlas :: String -> Maybe (GLpoint2D, GLpoint2D)
atlas tileName =
    case tileName of 
        "creature" -> Just ((1,1),wh)
        "stairs" -> Just ((2,1),wh)
        "ground" -> Just ((3,1),wh)
        "wall" -> Just ((4,1),wh)
        "empty" -> Just((5,1),wh)
        "item" -> Just((6,1),wh)
        "building" -> Just((7,1),wh)
        "?" -> Just((8,1),wh)
        "black" -> Just((9,1),wh)
        "focus"-> Just((10,1),wh)
        _ -> Nothing
    where wh = (32,32)
    
loadTexture :: String -> IO ()
loadTexture imagePath = do
    [textureName] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just textureName
    GL.textureFilter  GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    image <- JP.readImage imagePath
    case image of 
        (Left s) -> do print s
        (Right (JP.ImageRGBA8 (JP.Image width height dat))) -> do
            -- Access the data vector pointer
            unsafeWith dat $ \ptr ->
                GL.texImage2D GL.Texture2D NoProxy 0 GL.RGBA8 
                    (GL.TextureSize2D (toEnum width) (toEnum height))
                    0 (PixelData GL.RGBA UnsignedByte ptr)
        (Right _) -> do
            print "image not found"