module Texture2D
where

import qualified Codec.Image.DevIL as IL
import Data.Array.Unboxed
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as OldGL
import Graphics.Rendering.OpenGL.GL (($=),($=!))
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GL
import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign (nullPtr, withArray, plusPtr, sizeOf, castPtr, Ptr, withMany)

import Data.Bitmap.OpenGL
import Codec.Image.STB

loadTextureOld :: FilePath -> OldGL.TextureUnit -> IO (OldGL.TextureObject, OldGL.TextureUnit)
loadTextureOld path unit = do 
  e  <- loadImage path
  case e of
    Left err -> error $ "loadTextureOld: " ++ err
    Right im -> do
      OldGL.activeTexture $= unit
      t <- makeSimpleBitmapTexture im
      return (t,unit)

enableTextureOld :: (OldGL.TextureObject,OldGL.TextureUnit) -> IO ()
enableTextureOld (t,u) = do
  OldGL.activeTexture $= u
  OldGL.texture OldGL.Texture2D $= OldGL.Enabled
  OldGL.textureBinding OldGL.Texture2D $= Just t

enableTexture :: (GL.GLuint, GL.GLenum) -> IO ()
enableTexture (t,u) = do
  GL.glActiveTexture u
  GL.glEnable GL.gl_TEXTURE_2D
  GL.glBindTexture GL.gl_TEXTURE_2D t

loadTexture :: FilePath -> IO GL.GLuint
loadTexture path = do
  uarr <- IL.readImage path
  let es = elems uarr
      (w,h,c) = snd . bounds $ uarr
  print $ "w: " ++ show w ++ " h: " ++ show h
  texture2d (fromIntegral w) (fromIntegral h) es

loadTexture' :: FilePath -> GL.GLenum -> IO (GL.GLuint, GL.GLenum)
loadTexture' path unit = do
  GL.glActiveTexture unit 
  t <- loadTexture path
  return (t, unit)

texture2d :: (Storable a) => GL.GLsizei -> GL.GLsizei -> [a] -> IO GL.GLuint
texture2d w h xs = 
  let level = 0
      internalFormat = fromIntegral GL.gl_RGBA
      border = 0
      format = GL.gl_RGBA
      ty = GL.gl_UNSIGNED_BYTE
      target = GL.gl_TEXTURE_2D
  in do 
        t <- fmap head $ allocaArray 1 $ \buf -> GL.glGenTextures 1 buf >> peekArray 1 buf
        GL.glBindTexture target t
        withArray xs $ \ptr ->
          GL.glTexImage2D target level internalFormat w h border format ty ptr
        return t


