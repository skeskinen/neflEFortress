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

type Menu = [String]

gameMenu :: Menu
gameMenu = [
    "",
    "p: pause",
    "arrows: move focus",
    "<>: up/down",
    "g: restart"
    ]
    
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
            
tileAtlas :: String -> Maybe (GLpoint2D, GLpoint2D)
tileAtlas tileName =
    case tileName of 
        "stairs" -> Just ((2,1),wh)
        "ground" -> Just ((3,1),wh)
        "wall" -> Just ((4,1),wh)
        "empty" -> Just((5,1),wh)
        "item" -> Just((6,1),wh)
        "building" -> Just((7,1),wh)
        "?" -> Just((8,1),wh)
        "black" -> Just((9,1),wh)
        "focus"-> Just((10,1),wh)
        "creature1" -> Just ((1,2),wh)
        "creature2" -> Just ((2,2),wh)
        _ -> Nothing
    where wh = (32,32)

charAtlas :: Char -> Maybe (GLpoint2D, GLpoint2D)
charAtlas c
    | c `elem` ['0'..'9'] = Just (((fromIntegral $ digitToInt c)+1, 32),(64,32))
    | c `elem` ['a'..'z'] = Just (((fromIntegral $ ord c)-86, 32),(64,32))
    | c `elem` ['A'..'Z'] = Just (((fromIntegral $ ord c)-28, 32),(64,32))
    | c `elem` "!#$%&'()*+,-./" = Just (((fromIntegral $ ord c)-32, 31),(64,32))
    | c `elem` ":;<=>?@" = Just (((fromIntegral $ ord c)-42, 31),(64,32))
    | otherwise = Nothing

drawMenu :: Menu -> GLpoint2D -> GLpoint2D -> IO()
drawMenu [] _ _ = return()
drawMenu (x:xs) (destX, destY) (w, h) = do
    drawString x (destX, destY) (w,h)
    drawMenu xs (destX, destY+h) (w,h)

drawImage :: String -> GLpoint2D -> GLpoint2D -> IO()
drawImage tileName = drawGeneric (tileAtlas tileName)

drawString :: String -> GLpoint2D -> GLpoint2D  -> IO()
drawString [] _ _ = return()
drawString (c:cs) (destX, destY) (w, h) = do
    drawGeneric (charAtlas c) (destX, destY) (w, h)
    drawString cs ((destX + w),destY) (w,h)
    
drawGeneric :: Maybe (GLpoint2D, GLpoint2D) 
    -> GLpoint2D -> GLpoint2D -> IO()
drawGeneric imagePos (destX, destY) (w, h) =
    case imagePos of 
        Just ((imageX, imageY),(atlasW, atlasH)) -> do
            GL.texCoord $ texCoord2 ((imageX-1)/atlasW)  (imageY/atlasH)
            GL.vertex   $ vertex3 destX         (destY + h) 0
            GL.texCoord $ texCoord2 ((imageX-1)/atlasW)  ((imageY-1)/atlasH)
            GL.vertex   $ vertex3 destX         destY       0
            GL.texCoord $ texCoord2 (imageX/atlasW)      ((imageY-1)/atlasH)
            GL.vertex   $ vertex3 (destX + w)   destY       0
            GL.texCoord $ texCoord2 (imageX/atlasW)      (imageY/atlasH)
            GL.vertex   $ vertex3 (destX + w)   (destY + h) 0
        Nothing -> return () 

