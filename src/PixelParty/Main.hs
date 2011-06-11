module PixelParty.Main (pixelparty) where

-- TODO:
--   add ping-pong flag:
--     render to texture. supply that texture as an input for the next frame

import Control.Exception (bracket_)
import Control.Monad (when, forM_, unless)
import Control.Monad.State
import qualified Data.Map as M

import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign (Storable, nullPtr, withArray, sizeOf, castPtr, Ptr)
import Foreign.C.String (peekCString, withCAString)

import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.List (foldl')

import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Time as T
import qualified Graphics.UI.SDL as SDL

import PixelParty.CmdLine
import PixelParty.Types
import PixelParty.ShaderIncludes
import PixelParty.Shader
import qualified PixelParty.Texture2D as Tex
import qualified PixelParty.Window as W

-- -----------------------------------------------------------------------------
-- helper
-- from OpenGL-2.4.0.1/Graphics/Rendering/OpenGL/GL/Shaders.hs

getString :: GL.GLenum -> IO String
getString e = GL.glGetString e >>= \ptr ->
    if ptr == nullPtr then return "" else peekCString . castPtr $ ptr

buffer :: (Storable a) => GL.GLenum -> GL.GLenum -> [a] -> IO GL.GLuint
buffer target usage xs = do 
  b <- gen GL.glGenBuffers
  GL.glBindBuffer target b
  withArray xs $ \ptr -> GL.glBufferData target (size xs) ptr usage
  return b

-- -----------------------------------------------------------------------------

gens :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> Int -> IO GL.GLuint
gens what n = fmap head $ allocaArray n $ \buf -> what (fromIntegral n) buf >> peekArray n buf

gen :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> IO GL.GLuint
gen what = gens what 1

createVBO :: P ()
createVBO = 
  let vertices :: [GL.GLfloat]
      vertices = [ -1.0, -1.0, 0.0, 1.0 ,
                    -1.0, 1.0, 0.0, 1.0 ,
                    1.0, 1.0, 0.0, 1.0 ,
                    1.0, -1.0, 0.0, 1.0 ]
      indices :: [GL.GLubyte]
      indices = [0,1,2,0,2,3]

      size as = fromIntegral (length as * sizeOf (head as))
  in do
    vao <- io $ gen GL.glGenVertexArrays
    io $ GL.glBindVertexArray vao

    vbo <- io $ buffer GL.gl_ARRAY_BUFFER GL.gl_STATIC_DRAW vertices

    io $ do
      GL.glVertexAttribPointer 0 4 GL.gl_FLOAT (fromIntegral GL.gl_FALSE) 0 nullPtr
      GL.glEnableVertexAttribArray 0

    ibo <- io $ buffer GL.gl_ELEMENT_ARRAY_BUFFER GL.gl_STATIC_DRAW indices 
   
    modify (\s -> s{vaoId = vao,arrayBuffer = vbo,elementBuffer = ibo})

    io $ do 
      errorCheckValue <- GL.glGetError
      --when (errorCheckValue /= GL.gl_NO_ERROR)
      --  GLUT.reportErrors
      return ()

createTextures :: [FilePath] -> P ()
createTextures imgs = do
  io $ GL.glEnable GL.gl_TEXTURE
  ts <- io $ mapM (uncurry Tex.loadTexture) (zip imgs [GL.gl_TEXTURE0..])
  io $ mapM_ Tex.enableTexture ts
  modify (\s -> s {textures = ts})

createShaders :: CmdLine -> P ()
createShaders opts = do
  let path = ".":include opts

  vs <- io $ if null (vshader opts) then return vertexShader
              else includeFiles path =<< readFile (vshader opts)
  fs <- io $ includeFiles path =<< readFile (fshader opts)

  errorCheckValue <- io $ GL.glGetError

  (v,f,p) <- io $ loadProgram vs fs
  io $ GL.glUseProgram p
  modify (\s -> s{programId = p, vertexShaderId = v, fragmentShaderId = f})

  let names = ["resolution","time","mouse","tex0","tex1","tex2","tex3"]
  ls <- io $ mapM (uniformLoc p) names
  let m = M.fromList $ zip names ls
  modify (\s -> s { uniforms = m })

  io $ forM_ [0,1,2,3] $ \i ->
    case M.lookup ("tex"++show i) m of
      Nothing -> return ()
      Just loc -> GL.glUniform1i loc i

initialize :: CmdLine -> P ()
initialize opts = do
  createShaders opts
  createVBO
  createTextures (tex opts)
  io $ do
    GL.glDepthFunc (depthTest defaultPartyState)
    GL.glClearColor 0.0 0.0 0.0 0.0

{-
keyboardMouseCB ref k ks mod pos =
  case (k,ks) of
    (GLUT.Char '\ESC', GLUT.Down) -> GLUT.leaveMainLoop
    (GLUT.Char 'r', GLUT.Down) -> reloadProgram opts ref
    --(GLUT.Char 'R', GLUT.Down) -> resetTime opts ref
    _ -> return () -- putStrLn ("k: " ++ show k ++ ", ks: " ++ show ks)

mouseCB ref (GLUT.Position x y) = do
  state <- readIORef ref
  case M.lookup "mouse" (uniforms state) of
    Nothing -> return ()
    Just loc -> GL.glUniform2f loc (fromIntegral x) (fromIntegral y)
-}

windowTitle :: String
windowTitle = "pixelparty"

{-
timerFunction :: PRef -> IO ()
timerFunction r = do
  state <- readIORef r
  let tempString = windowTitle ++ ": " ++ show (4 * frameCount state) ++ " Frames Per Second @ " ++ show (currentWidth state) ++ " x " ++ show (currentHeight state)
  GLUT.windowTitle GLUT.$= tempString

  modifyIORef r (\s -> s { frameCount = 0 })
  GLUT.addTimerCallback 250 (timerFunction r)
-}

size as = fromIntegral (length as * sizeOf (head as))

pixelparty :: CmdLine -> IO ()
pixelparty opts = do
  win <- W.openWindow windowTitle (width opts , height opts)
  now <- T.getCurrentTime
  let st = defaultPartyState 
            { vertFile = vshader opts
            , fragFile = fshader opts
            , startTime = now }
  runP st $ do
    initialize opts
    mainloop (process >> render)
    cleanup
  return ()
    where
      mainloop a = gets done >>= flip unless (a >> mainloop a)

      process = io SDL.pollEvent >>= handle

-- -----------------------------------------------------------------------------
-- | Event handler
handle :: SDL.Event -> P ()
handle SDL.NoEvent = return ()
handle SDL.Quit = stop
handle (SDL.KeyDown keysym) = case SDL.symKey keysym of
                                SDL.SDLK_ESCAPE -> stop
                                _ -> return ()
handle (SDL.VideoResize w h) = do 
  modify (\s -> s {currentWidth = w, currentHeight = h})
  s <- io $ W.resizeWindow w h
  io $ GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
  us <- gets uniforms
  io $ case M.lookup "resolution" us of
    Nothing -> return ()
    Just loc -> GL.glUniform2f loc (fromIntegral w) (fromIntegral h)
handle _ = return ()

stop :: P ()
stop = modify (\s -> s {done = True})

cleanup :: P ()
cleanup = do
  state <- get
  errorCheckValue <- io $ GL.glGetError

  io $ GL.glUseProgram 0
  io $ GL.glDeleteProgram (programId state)

  io $ GL.glDisableVertexAttribArray 0

  io $ GL.glBindBuffer GL.gl_ARRAY_BUFFER 0
  io $ withArrayLen [arrayBuffer state] $ GL.glDeleteBuffers . fromIntegral

  io $ GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER 0
  io $ withArrayLen [elementBuffer state] $ GL.glDeleteBuffers . fromIntegral

  io $ GL.glBindVertexArray 0
  io $ withArrayLen [vaoId state] $
    GL.glDeleteVertexArrays . fromIntegral

  io $ withArrayLen (map fst (textures state)) $
    GL.glDeleteTextures . fromIntegral

  errorCheckValue <- io $ GL.glGetError
{-
  when (errorCheckValue /= GL.gl_NO_ERROR) $ do
    putStrLn "ERROR: Could not destroy the GL objects"
    GLUT.reportErrors
-}
  return ()

render :: P ()
render = do
  modify (\s -> s{frameCount = frameCount s + 1})
  state <- get
  io $ do
    now <- T.getCurrentTime
    let t = T.diffUTCTime now (startTime state)
    case M.lookup "time" (uniforms state) of
      Nothing -> return ()
      Just loc -> GL.glUniform1f loc (realToFrac t)
    GL.glClear . sum . map fromIntegral $ [GL.gl_COLOR_BUFFER_BIT, GL.gl_DEPTH_BUFFER_BIT]
    GL.glDrawElements GL.gl_TRIANGLES 6 GL.gl_UNSIGNED_BYTE nullPtr
    SDL.glSwapBuffers
