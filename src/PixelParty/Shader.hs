-- -----------------------------------------------------------------------------
-- |
-- Module      :  PixelParty.Shader
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Helper functions to load GLSL shader.
--
--------------------------------------------------------------------------------

module PixelParty.Shader
  ( setShaderSource
  , loadProgram
  , loadProgramFrom
  --, reloadProgram
  , uniformLoc
  , shader
  , vertexShader
  , linkStatus
  , compileStatus
  ) where

import PixelParty.Types
import PixelParty.ShaderIncludes
import Control.Monad (forM_, mapM, when, (>=>))
import Graphics.GL.Core41 as GL
import Graphics.GL.Types as GL
import Foreign (withArray, castPtr, Ptr, withMany, allocaArray, peekArray
      , nullPtr)
import Foreign.C.String (withCAStringLen, withCAString, peekCAStringLen, peekCAString)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- shaderInfoLog

type GLStringLen = (Ptr GL.GLchar, GL.GLsizei)
type GLShader = GL.GLuint

-- | 'setShaderSource' is a wrapper around glShaderSource.
setShaderSource :: GLShader -> [String] -> IO ()
setShaderSource shader srcs = do
   let len = fromIntegral . length $ srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            GL.glShaderSource shader len charBufsBuf lengthsBuf

-- | 'shader'
shader :: GL.GLenum -> String -> IO GLShader
shader ty src = do
  s <- GL.glCreateShader ty
  setShaderSource s [src]
  GL.glCompileShader s
  return s

-- | returns the uniform location within a program object.
uniformLoc :: GLProgram -> String -> IO GL.GLint
uniformLoc (GLProgram p) name = 
  withCAString name (GL.glGetUniformLocation p . castPtr)

-- | load a GLProgram.
loadProgram :: String -> String -> Maybe String ->
  IO (GLVertexShader, GLFragmentShader, GLGeometryShader, GLProgram)
loadProgram vs fs mgs = do
  progId <- GL.glCreateProgram
  v <- shader GL.GL_VERTEX_SHADER vs
  f <- shader GL.GL_FRAGMENT_SHADER fs
  GL.glAttachShader progId v 
  GL.glAttachShader progId f 
  g <- case mgs of 
    Nothing -> return 0
    Just gs -> do
      g <- shader GL.GL_GEOMETRY_SHADER gs
      GL.glAttachShader progId g
      return g

  GL.glLinkProgram  progId
  return (GLVertexShader v,GLFragmentShader f,GLGeometryShader g,GLProgram progId)

-- | load a program from files containing a vertex shader, a fragment shader and
-- maybe a geometry shader
loadProgramFrom :: [FilePath] -> FilePath -> FilePath -> FilePath ->
  IO (GLVertexShader, GLFragmentShader, GLGeometryShader, GLProgram)
loadProgramFrom path vs fs gs = do
  vshader <- if null vs then return vertexShader else includeFiles path =<< readFile vs
  fshader <- includeFiles path =<< readFile fs
  gshader <- if null gs then return Nothing else return . Just =<< includeFiles path =<< readFile gs
  loadProgram vshader fshader gshader

vertexShader :: String
vertexShader =
     "#version 330\n"
  ++ "layout(location=0) in vec4 in_position;\n"
  ++ "smooth out vec2 tc;\n"
  ++ "smooth out vec3 origin;\n"
  ++ "smooth out vec3 raydir;\n"
  ++ "void main(void)\n"
  ++ "{\n"
  ++ "  tc = in_position.xy;\n"
  ++ "  gl_Position = in_position;\n"
  ++ "  origin = vec3(0.0);\n"
  ++ "  raydir = vec3(in_position.x * 1.66667, in_position.y, -1.0);\n"
  ++ "}\n"

fragmentShader :: String
fragmentShader =
     "#version 330\n"
  ++ "smooth in vec2 tc;\n"
  ++ "out vec4 fragColor;\n"
  ++ "void\n"
  ++ "main(void)\n"
  ++ "{\n"
  ++ "  fragColor = vec4(tc,0.0,1.0);\n"
  ++ "}\n"

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act = withCAStringLen s $ 
  \(p,len) -> act (castPtr p, fromIntegral len)

maybeSetUniform :: Maybe GL.GLint -> (GL.GLint -> t -> IO ()) -> t -> IO ()
maybeSetUniform m set val = maybe (return ()) (`set` val) m

-- reloadProgram :: IO (Eiter String (Vert,Frag,Prog))
--
-- delete (Vert,Frag,Prog) -> IO ()
-- case rp of
--   Left log -> putStrLn log
--   Right (v,f,p) -> 
--      do  delete old 
--          glUseProgram p 
--          saveInPref (v,f,p)

-- | Returns the information log for a shader object.
-- The function 'shaderInfoLog' is a wrapper around 'glGetShaderInfoLog'.
shaderInfoLog :: GLShader -> IO String
shaderInfoLog shader = do
  maxLength <- fmap (fromIntegral . head) $ allocaArray 1 $ \buf -> 
    GL.glGetShaderiv shader GL.GL_INFO_LOG_LENGTH buf >> peekArray 1 buf
  allocaArray (fromIntegral maxLength) $ \infoLogPtr -> do
      GL.glGetShaderInfoLog shader maxLength nullPtr infoLogPtr
      peekCAString (castPtr infoLogPtr)

-- | Returns the information log for a program object.
-- The function 'programInfoLog' is a wrapper around 'glGetProgramInfoLog'.
programInfoLog :: GLProgram -> IO String
programInfoLog (GLProgram program) = do
  maxLength <- fmap (fromIntegral . head) $ allocaArray 1 $ \buf ->
    GL.glGetProgramiv program GL.GL_INFO_LOG_LENGTH buf >> peekArray 1 buf
  allocaArray (fromIntegral maxLength) $ \infoLogPtr -> do
      GL.glGetProgramInfoLog program maxLength nullPtr infoLogPtr
      peekCAString (castPtr infoLogPtr)

-- | Was the shader successfully compiled?
compileStatus :: GLShader -> IO Bool
compileStatus s = 
  fmap ((==fromIntegral GL.GL_TRUE). head) $ allocaArray 1 $ \buf ->
    GL.glGetShaderiv s GL.GL_COMPILE_STATUS buf >> peekArray 1 buf

-- | Was the program successfully linked?
linkStatus :: GLProgram -> IO Bool
linkStatus (GLProgram p) = 
  fmap ((==fromIntegral GL.GL_TRUE). head) $ allocaArray 1 $ \buf ->
    GL.glGetProgramiv p GL.GL_LINK_STATUS buf >> peekArray 1 buf
