{-# LANGUAGE ScopedTypeVariables #-}
module Texture2D
  ( loadTexture
  , enableTexture
  ) where

import Control.Applicative ((<$>), pure)
import Data.Array.Unboxed
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array (withArray, allocaArray, peekArray, withArrayLen)
import Foreign (nullPtr, plusPtr, sizeOf, castPtr, Ptr, withMany)
import Data.Array.MArray (thaw)
import Data.Array.Storable (withStorableArray)

import Data.Bitmap.IO
import Codec.Image.STB (loadImage)

type GLTextureUnit = GL.GLenum
type GLTextureObject = GL.GLuint

loadTexture :: FilePath -> GLTextureUnit -> IO (GLTextureObject, GLTextureUnit)
loadTexture path u = do 
  e  <- loadImage path
  case e of
    Left err -> error $ "loadTexture: " ++ err
    Right bm -> do
      GL.glActiveTexture u
      t <- fmap head $ allocaArray 1 (\buf -> GL.glGenTextures 1 buf >> peekArray 1 buf)
      GL.glBindTexture GL.gl_TEXTURE_2D t
      GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MIN_FILTER 
                                        (fromIntegral GL.gl_LINEAR)
      GL.glTexParameteri GL.gl_TEXTURE_2D GL.gl_TEXTURE_MAG_FILTER 
                                        (fromIntegral GL.gl_LINEAR)

      withBitmap bm $ \(width,height) nchn pad ptr -> do
        let ty = marshalPixelComponent bm
            (pf,pif) = formatPlusInternalFormat bm
            (w,h) = (fromIntegral width, fromIntegral height)
            alignment = fromIntegral (bitmapRowAlignment bm)
        GL.glPixelStorei GL.gl_UNPACK_ALIGNMENT alignment
        GL.glTexImage2D GL.gl_TEXTURE_2D 0 pif w h 0 pf ty ptr

      return (t,u)

enableTexture :: (GL.GLuint, GLTextureUnit) -> IO ()
enableTexture (t,u) = do
  GL.glActiveTexture u
  GL.glEnable GL.gl_TEXTURE_2D
  GL.glBindTexture GL.gl_TEXTURE_2D t

-- -----------------------------------------------------------------------------

marshalPixelComponent :: forall t.PixelComponent t => Bitmap t -> GL.GLenum
marshalPixelComponent _ = case pixelComponentType (undefined::t) of
  PctWord8  -> GL.gl_UNSIGNED_BYTE
  PctWord16 -> GL.gl_UNSIGNED_SHORT
  PctWord32 -> GL.gl_UNSIGNED_INT
  PctFloat  -> GL.gl_FLOAT

formatPlusInternalFormat :: forall t. PixelComponent t => Bitmap t -> (GL.GLenum, GL.GLint)
formatPlusInternalFormat bm = 
  case pixelComponentType (undefined::t) of 
        PctWord8 -> case bitmapNChannels bm of
          1 -> (GL.gl_ALPHA, fromIntegral GL.gl_ALPHA8)
          2 -> (GL.gl_LUMINANCE_ALPHA, fromIntegral GL.gl_LUMINANCE8_ALPHA8)
          3 -> (GL.gl_RGB, fromIntegral GL.gl_RGB8)
          4 -> (GL.gl_RGBA, fromIntegral GL.gl_RGBA8)  
        _ -> case bitmapNChannels bm of
          1 -> (GL.gl_ALPHA, fromIntegral GL.gl_ALPHA)
          2 -> (GL.gl_LUMINANCE_ALPHA, fromIntegral GL.gl_LUMINANCE_ALPHA)
          3 -> (GL.gl_RGB, fromIntegral GL.gl_RGB)
          4 -> (GL.gl_RGBA, fromIntegral GL.gl_RGBA)  

