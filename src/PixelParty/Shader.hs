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
import Control.Monad (forM_, mapM, when)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Foreign (withArray, castPtr, Ptr, withMany, allocaArray, peekArray)
import Foreign.C.String (withCAStringLen, withCAString)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- shaderInfoLog

type GLStringLen = (Ptr GL.GLchar, GL.GLsizei)
type GLShader = GL.GLuint

-- | setShaderSource
setShaderSource :: GLShader -> [String] -> IO ()
setShaderSource shader srcs = do
   let len = fromIntegral . length $ srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            GL.glShaderSource shader len charBufsBuf lengthsBuf

shader :: GL.GLenum -> String -> IO GLShader
shader ty src = do
  s <- GL.glCreateShader ty
  setShaderSource s [src]
  GL.glCompileShader s
  return s

uniformLoc :: GLProgram -> String -> IO GL.GLint
uniformLoc p name = withCAString name (GL.glGetUniformLocation p . castPtr)

loadProgram :: String -> String -> 
  IO (GLVertexShader, GLFragmentShader, GLProgram)
loadProgram vs fs = do
  v <- shader GL.gl_VERTEX_SHADER vs
  f <- shader GL.gl_FRAGMENT_SHADER fs
  progId <- GL.glCreateProgram
  GL.glAttachShader progId v 
  GL.glAttachShader progId f 
  GL.glLinkProgram  progId
  return (v,f,progId)

loadProgramFrom :: [String] -> FilePath -> FilePath -> 
  IO (GLVertexShader, GLFragmentShader, GLProgram)
loadProgramFrom path vs fs = do
  vshader <- if null vs then return vertexShader
              else includeFiles path =<< readFile vs
  fragmentShader <- includeFiles path =<< readFile fs
  loadProgram vshader fragmentShader

{-
reloadProgram :: CmdLine -> PRef -> IO ()
reloadProgram opts ref = do
  state <- readIORef ref
  let oldProgram = programId state
  --when (0 /= programId state) 
  --  (GL.glDeleteProgram . programId $ state)

  let path = ".":include opts
  (v,f,p) <- loadProgramFrom path (vertFile state) (fragFile state)
  modifyIORef ref (\state -> state {vertexShaderId = v, fragmentShaderId = f, programId = p})
  GL.glUseProgram p

  -- \val loc -> GL.glUniform1i loc val == flip GL.glUniform1i
  forM_ [0,1,2,3] $ \i ->
    case M.lookup ("tex"++show i) (uniforms state) of
      Nothing -> return ()
      Just loc -> GL.glUniform1i loc i

  w <- currentWidth `fmap` readIORef ref
  h <- currentHeight `fmap` readIORef ref
  case M.lookup "resolution" (uniforms state) of
    Nothing -> return ()
    Just loc -> GL.glUniform2f loc (fromIntegral w) (fromIntegral h)

  print "reloadProgram : glBindVertexArray"
-}

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

-- glGetShaderInfoLog

-- shaderInfoLog :: GLShader -> IO String
-- shaderInfoLog sh =
-- programInfoLog :: GLProgram -> IO String
-- programInfoLog p =

compileStatus :: GLShader -> IO Bool
compileStatus s = 
  fmap ((==fromIntegral GL.gl_TRUE). head) $ allocaArray 1 $ \buf -> GL.glGetShaderiv s GL.gl_COMPILE_STATUS buf >> peekArray 1 buf

linkStatus :: GLProgram -> IO Bool
linkStatus p = 
  fmap ((==fromIntegral GL.gl_TRUE). head) $ allocaArray 1 $ \buf -> GL.glGetProgramiv p GL.gl_LINK_STATUS buf >> peekArray 1 buf
