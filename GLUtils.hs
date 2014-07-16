module GLUtils where 

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad

type GLpoint2D = (GLfloat, GLfloat)

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

texCoord2 :: GLfloat -> GLfloat -> GL.TexCoord2 GLfloat
texCoord2 = GL.TexCoord2
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

setupUI :: IO()
setupUI = do
    GLFW.initialize
    -- open window
    GLFW.openWindow (GL.Size 800 800) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "GLFW"
        
    -- set the color to clear background
    GL.clearColor $= Color4 0 0 0 0
    -- load texture
    [textureName] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just textureName
    GL.textureFilter  GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
    GLFW.loadTexture2D "tileset.tga" [OriginUL]
    GL.texture GL.Texture2D $= Enabled
    
    -- enable transparency
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)


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
        _ -> Nothing
    where wh = (32,32)
    