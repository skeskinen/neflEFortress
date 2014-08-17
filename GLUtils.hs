module GLUtils where 

import Prelude hiding (init)
import System.IO
import Control.Monad
import Data.Vector.Storable (unsafeWith)
import Control.Concurrent.STM
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW
import Data.Char
import qualified Codec.Picture as JP

------ datatypes ------
type GLpoint2D = (GLfloat, GLfloat)

vertex3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3 = Vertex3

texCoord2 :: GLfloat -> GLfloat -> TexCoord2 GLfloat
texCoord2 = TexCoord2
 
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

charCallback :: TQueue Button -> Window -> Char -> IO () 
charCallback que _ char = atomically $ writeTQueue que (CKey char)

keyCallback :: TQueue Button -> Window -> Key -> Int 
               -> KeyState -> ModifierKeys -> IO ()
keyCallback que _ key _ s _ = when (s /= KeyState'Released) (atomically $ writeTQueue que (BKey key))

windowSizeCallback :: Window -> Int -> Int -> IO ()
windowSizeCallback _ = prepareViewport

------ Ui and Window preparation ------
setupUi :: IO Window
setupUi = do
    setErrorCallback (Just errorCallback)
    _ <- init
    -- open window
    Just win <- createWindow 800 800 "neflEFortress" Nothing Nothing    -- [DisplayAlphaBits 8] Window
    makeContextCurrent (Just win)
    -- set the color to clear background
    clearColor $= Color4 0 0 0 0
    -- load texture
    loadTexture "tileset.png"
    texture Texture2D $= Enabled
    prepareViewport 800 800
    
    -- enable transparency
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    return win

prepareViewport:: Int -> Int -> IO ()
prepareViewport w h = do
    viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (realToFrac w) (realToFrac h) 0

------ textures ------
loadTexture :: String -> IO ()
loadTexture imagePath = do
    [textureName] <- genObjectNames 1
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
            
tileAtlas :: String -> Maybe (GLpoint2D, GLpoint2D)
tileAtlas tileName =
    case tileName of
        "white" ->      Just((1,1),wh)
        "stairs" ->     Just((2,1),wh)
        "ground" ->     Just((3,1),wh)
        "wall" ->       Just((4,1),wh)
        "empty" ->      Just((5,1),wh)
        "item" ->       Just((6,1),wh)
        "building" ->   Just((7,1),wh)
        "?" ->          Just((8,1),wh)
        "black" ->      Just((9,1),wh)
        "focus"->       Just((10,1),wh)
        "creature1" ->  Just((1,2),wh)
        "creature2" ->  Just((2,2),wh)
        "creature3" ->  Just((3,2),wh)
        "shoe"      ->  Just((1,3),wh)
        "bag"       ->  Just((2,3),wh)
        _ -> Nothing
    where wh = (32,32)

charAtlas :: Char -> Maybe (GLpoint2D, GLpoint2D)
charAtlas c
    | c `elem` ['0'..'9'] = Just ((fromIntegral (digitToInt c)+1, 32),(64,32))
    | c `elem` ['a'..'z'] = Just ((fromIntegral (ord c)-86, 32),(64,32))
    | c `elem` ['A'..'Z'] = Just ((fromIntegral (ord c)-28, 32),(64,32))
    | c `elem` "!#$%&'()*+,-./" = Just ((fromIntegral (ord c)-32, 31),(64,32))
    | c `elem` ":;<=>?@" = Just ((fromIntegral (ord c)-42, 31),(64,32))
    | otherwise = Nothing

------ drawing ------
drawMenu :: Menu -> GLpoint2D -> GLpoint2D -> IO()
drawMenu ([],_) _ _ = return()
drawMenu (x : xs, i) (destX, destY) (w, h) = do
    if i == 0
        then do 
            color $ color3 0.5 1 0.5
            drawString ('@':x) (destX, destY) (w,h)
            color $ color3 1 1 1
        else drawString (' ':x) (destX, destY) (w,h)
    drawMenu (xs,i-1) (destX, destY+h) (w,h)

drawImage :: String -> GLpoint2D -> GLpoint2D -> IO()
drawImage tileName = drawGeneric (tileAtlas tileName)

drawColored :: String -> GLpoint2D -> GLpoint2D -> Color3 GLfloat -> IO()
drawColored tileName p r col = do
    color col
    drawImage tileName p r
    color white 

drawString :: String -> GLpoint2D -> GLpoint2D  -> IO()
drawString [] _ _ = return()
drawString (c:cs) (destX, destY) (w, h) = do
    drawGeneric (charAtlas c) (destX, destY) (w, h)
    drawString cs (destX + w, destY) (w, h)
    
drawGeneric :: Maybe (GLpoint2D, GLpoint2D) 
    -> GLpoint2D -> GLpoint2D -> IO()
drawGeneric imagePos (destX, destY) (w, h) =
    case imagePos of 
        Just ((imageX, imageY),(atlasW, atlasH)) -> do
            texCoord $ texCoord2 ((imageX-1)/atlasW)  (imageY/atlasH)
            vertex   $ vertex3 destX         (destY + h) 0
            texCoord $ texCoord2 ((imageX-1)/atlasW)  ((imageY-1)/atlasH)
            vertex   $ vertex3 destX         destY       0
            texCoord $ texCoord2 (imageX/atlasW)      ((imageY-1)/atlasH)
            vertex   $ vertex3 (destX + w)   destY       0
            texCoord $ texCoord2 (imageX/atlasW)      (imageY/atlasH)
            vertex   $ vertex3 (destX + w)   (destY + h) 0
        Nothing -> return () 

