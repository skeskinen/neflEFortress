module GLUtils where 

import Data.IORef
import Control.Monad
import System.IO
import System.Exit 
import Data.Vector.Storable (unsafeWith)
import Control.Concurrent.STM
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW as GLFW
import Data.Char
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

charCallback :: TQueue Char -> GLFW.Window -> Char -> IO () 
charCallback que win char = atomically $ writeTQueue que char

keyCallback :: TQueue GLFW.Key -> GLFW.Window -> GLFW.Key -> Int 
               -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback que win key i s mod = if (s == GLFW.KeyState'Pressed)
    then do atomically $ writeTQueue que key
    else return()

windowSizeCallback :: Window -> Int -> Int -> IO ()
windowSizeCallback win w h = prepareViewport w h

-- UI and Window preparation  
setupUI :: IO GLFW.Window
setupUI = do
    GLFW.setErrorCallback (Just errorCallback)
    GLFW.init
    -- open window
    Just win <- GLFW.createWindow 800 800 "neflEFortress" Nothing Nothing    -- [GLFW.DisplayAlphaBits 8] GLFW.Window
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

charAtlas :: Char -> Maybe (GLpoint2D, GLpoint2D)
charAtlas c
    | c `elem` ['0'..'9'] = Just ((1 + (fromIntegral $ digitToInt c), 32),(64,32))
    | c `elem` ['a'..'z'] = Just (((fromIntegral $ ord c)-86, 32),(64,32))
    | c `elem` ['A'..'Z'] = Just (((fromIntegral $ ord c)-28, 32),(64,32))
    | c `elem` "!#$%&'()*+,-./@" = Just (((fromIntegral $ ord c)-32, 31),(64,32))
    | c `elem` ":;<=>?" = Just (((fromIntegral $ ord c)-42, 31),(64,32))
    | otherwise = Nothing
    


drawImage :: GLpoint2D -> GLpoint2D -> String -> IO()
drawImage (destX1, destY1) (destX2, destY2) tileName = do
    let tilePos = atlas tileName
    case tilePos of 
        Just ((tileX, tileY),(atlasW, atlasH)) -> do
            GL.texCoord $ texCoord2 ((tileX-1)/atlasW)  (tileY/atlasH)
            GL.vertex   $ vertex3 destX1 destY2 0
            GL.texCoord $ texCoord2 ((tileX-1)/atlasW)  ((tileY-1)/atlasH)
            GL.vertex   $ vertex3 destX1 destY1 0
            GL.texCoord $ texCoord2 (tileX/atlasW)      ((tileY-1)/atlasH)
            GL.vertex   $ vertex3 destX2 destY1 0
            GL.texCoord $ texCoord2 (tileX/atlasW)      (tileY/atlasH)
            GL.vertex   $ vertex3 destX2 destY2 0
        Nothing -> print "tilename not in atlas"

drawString :: GLpoint2D -> GLpoint2D -> String -> IO()
drawString (destX, destY) (w, h) (x:xs) = do
    let charPos = charAtlas x
    case charPos of 
        Just ((charX, charY),(atlasW, atlasH)) -> do
            GL.texCoord $ texCoord2 ((charX-1)/atlasW)  (charY/atlasH)
            GL.vertex   $ vertex3 destX         (destY + h) 0
            GL.texCoord $ texCoord2 ((charX-1)/atlasW)  ((charY-1)/atlasH)
            GL.vertex   $ vertex3 destX         destY       0
            GL.texCoord $ texCoord2 (charX/atlasW)      ((charY-1)/atlasH)
            GL.vertex   $ vertex3 (destX + w)   destY       0
            GL.texCoord $ texCoord2 (charX/atlasW)      (charY/atlasH)
            GL.vertex   $ vertex3 (destX + w)   (destY + h) 0
        Nothing -> return ()
    when (not (null xs)) (drawString ((destX + w),destY) (w,h) xs)


