module Main 
where

-- TODO:
--   add ping-pong flag:
--     render to texture. supply that texture as an input for the next frame

import Control.Applicative ((<$>), pure)
import Control.Monad (unless, forM_, forM)
import Control.Exception (bracket_)
import System.Directory (doesFileExist)
import System.Exit (exitWith,ExitCode(..))
import Control.Monad (when)
import qualified Data.Map as M

import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign (nullPtr, withArray, plusPtr, sizeOf, castPtr, Ptr, withMany)
import Foreign.C.String (peekCString, withCAStringLen, withCAString)

import Data.IORef (IORef(..), newIORef, modifyIORef, readIORef)
import Data.List (foldl')
import Data.Maybe (maybeToList)

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified PixelParty.Texture2D as T
import System.Console.CmdArgs
import CmdLine

type WindowHandle = GLUT.Window

-- -----------------------------------------------------------------------------
-- helper
-- from OpenGL-2.4.0.1/Graphics/Rendering/OpenGL/GL/Shaders.hs

type GLStringLen = (Ptr GL.GLchar, GL.GLsizei)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act = withCAStringLen s $ 
  \(p,len) -> act (castPtr p, fromIntegral len)

setShaderSource :: GL.GLuint -> [String] -> IO ()
setShaderSource shader srcs = do
   let len = fromIntegral . length $ srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            GL.glShaderSource shader len charBufsBuf lengthsBuf

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

type PRef = IORef PartyState

data PartyState = PartyState {
    currentTime   :: Float
  , mousePosition :: (Double,Double)

  -- opengl stuff
  , vertexShaderId :: GL.GLuint
  , fragmentShaderId :: GL.GLuint
  , programId       :: GL.GLuint
  , vaoId :: GL.GLuint
  , arrayBuffer   :: GL.GLuint
  , elementBuffer :: GL.GLuint
  , uniforms :: M.Map String GL.GLint
  -- , sampler
  , textures :: [(GL.GLuint,GL.GLenum)]
  , depthTest :: GL.GLenum
  -- , rasterizer -- viewport size, pos
  -- , draw
  , currentWidth :: Int
  , currentHeight :: Int
  , windowHandle :: WindowHandle
  , vertFile :: FilePath
  , fragFile :: FilePath
  }

defaultPartyState :: PartyState
defaultPartyState = PartyState
  { currentTime = 0
  , mousePosition = (0,0)
  , vertexShaderId = 0
  , fragmentShaderId = 0
  , programId = 0
  , vaoId = 0
  , arrayBuffer = 0
  , elementBuffer = 0
  , uniforms = M.empty
  , textures = []
  , depthTest = GL.gl_LESS
  , currentWidth = 600
  , currentHeight = 600
  , windowHandle = undefined
  , vertFile = ""
  , fragFile = ""
  }

gen :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> IO GL.GLuint
gen what = fmap head $ allocaArray 1 $ \buf -> what 1 buf >> peekArray 1 buf

reshapeCB :: PRef -> GLUT.Size -> IO ()
reshapeCB r s@(GLUT.Size w h) = do
  GL.glViewport 0 0 w h
  modifyIORef r (\s -> s {currentWidth = fromIntegral w, currentHeight = fromIntegral h})
  state <- readIORef r
  case M.lookup "resolution" (uniforms state) of
    Nothing -> return ()
    Just loc -> GL.glUniform2f loc (fromIntegral w) (fromIntegral h)

includeFiles :: [String] -> String -> IO String
includeFiles path = pp path . lines

-- inspired by cpphs
pp :: [String] -> [String] -> IO String
pp _ [] = return []
pp path (l@('#':x):xs) = 
  let ws = words x
      cmd = head ws
      line = tail ws
      file ('"':ns) = init ns
      file ('<':ns) = init ns
      file _ = error "unknown include file"
  in  case cmd of
    "include" -> do (content) <- readFirstFound (file (unwords line)) path
                    pp path (lines content ++ xs)
    _ -> ((l++"\n")++) <$> pp path xs
pp path (x:xs) = ((x++"\n")++) <$> pp path xs

readFirstFound :: String -> [String] -> IO String
readFirstFound name [] = error $ "Warning: Can't find file " ++ name
readFirstFound name (p:ps) = do 
  let file = p ++ '/':name
  b <- doesFileExist file
  if b then readFile file else readFirstFound name ps 

loadProgram :: [String] -> FilePath -> FilePath -> IO GL.GLuint
loadProgram path vs fs = do
  vshader <- if null vs then return vertexShader
              else includeFiles path =<< readFile vs
  v <- GL.glCreateShader GL.gl_VERTEX_SHADER
  setShaderSource v [vshader]
  GL.glCompileShader v

  fragmentShader <- includeFiles path =<< readFile fs
  f <- GL.glCreateShader GL.gl_FRAGMENT_SHADER
  setShaderSource f [fragmentShader]
  GL.glCompileShader f
  progId <- GL.glCreateProgram
  GL.glAttachShader progId v 
  GL.glAttachShader progId f 
  GL.glLinkProgram progId
  return progId

createVBO :: PRef -> IO ()
createVBO r = 
  let vertices :: [GL.GLfloat]
      vertices = [ -1.0, -1.0, 0.0, 1.0 ,
                    -1.0, 1.0, 0.0, 1.0 ,
                    1.0, 1.0, 0.0, 1.0 ,
                    1.0, -1.0, 0.0, 1.0 ]
      indices :: [GL.GLubyte]
      indices = [0,1,2,0,2,3]

      size as = fromIntegral (length as * sizeOf (head as))
  in do
    vao <- gen GL.glGenVertexArrays
    GL.glBindVertexArray vao

    vbo <- buffer GL.gl_ARRAY_BUFFER GL.gl_STATIC_DRAW vertices

    GL.glVertexAttribPointer 0 4 GL.gl_FLOAT (fromIntegral GL.gl_FALSE) 0 nullPtr
    GL.glEnableVertexAttribArray 0

    ibo <- buffer GL.gl_ELEMENT_ARRAY_BUFFER GL.gl_STATIC_DRAW indices 
   
    modifyIORef r (\s -> s{vaoId = vao,arrayBuffer = vbo,elementBuffer = ibo})

    errorCheckValue <- GL.glGetError
    when (errorCheckValue /= GL.gl_NO_ERROR)
      GLUT.reportErrors

createShaders :: CmdLine -> PRef -> IO ()
createShaders opts r = do
  let path = ".":include opts

  vshader <- if null (vshader opts) then return vertexShader
              else includeFiles path =<< readFile (vshader opts)
  fragmentShader <- includeFiles path =<< readFile (fshader opts)

  errorCheckValue <- GL.glGetError

  v <- GL.glCreateShader GL.gl_VERTEX_SHADER
  setShaderSource v [vshader]
  GL.glCompileShader v

  f <- GL.glCreateShader GL.gl_FRAGMENT_SHADER
  setShaderSource f [fragmentShader]
  GL.glCompileShader f

  progId <- GL.glCreateProgram
  GL.glAttachShader progId v 
  GL.glAttachShader progId f 
  GL.glLinkProgram progId
  GL.glUseProgram progId
  modifyIORef r (\s -> s{programId = progId, vertexShaderId = v, fragmentShaderId = f})

  let names = ["resolution","time","tex0","tex1","tex2","tex3"]
  ls <- forM names $ \name -> do
     withCAString name $ 
       GL.glGetUniformLocation progId . castPtr
  let m = M.fromList $ zip names ls
  modifyIORef r (\s -> s { uniforms = m })

initGL :: CmdLine -> WindowHandle -> IO PRef
initGL opts win = do
  r <- newIORef defaultPartyState
  GL.glDepthFunc (depthTest defaultPartyState)
  createShaders opts r
  createVBO r
  GL.glEnable GL.gl_TEXTURE
  --ts <- mapM (uncurry T.loadTexture') (zip (tex opts) $ [GL.gl_TEXTURE0 ..])
  ts <- mapM (uncurry T.loadTextureOld) (zip (tex opts) $ [GL.gl_TEXTURE0..])
  mapM_ T.enableTexture ts
  GL.glClearColor 0.0 0.0 0.0 0.0
  modifyIORef r (\s -> s {windowHandle = win, vertFile = vshader opts, fragFile = fshader opts})
  return r

openWindow :: String -> (Int,Int) -> IO WindowHandle
openWindow title (w,h) = do
  GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode GLUT.$= [ GLUT.DoubleBuffered
                              , GLUT.RGBAMode
                              , GLUT.WithDepthBuffer
                              , GLUT.WithAlphaComponent ] 
  GLUT.initialWindowSize GLUT.$= GLUT.Size (fromIntegral w) (fromIntegral h)
  GLUT.createWindow title

step :: PRef -> IO ()
step ref = modifyIORef ref (inc interval)
  where inc dt state = state { currentTime = currentTime state + fromIntegral dt}

withTimer :: IO () -> IO ()
withTimer io = GLUT.addTimerCallback interval (io >> withTimer io)

interval :: Int
interval = 10

simpleGLUTinit :: CmdLine -> (PRef -> IO ()) -> PRef -> IO ()
simpleGLUTinit opts party ref = do
  GLUT.displayCallback        GLUT.$= return ()
  GLUT.keyboardMouseCallback  GLUT.$= Just (keyboardMouseCB ref)
  GLUT.motionCallback         GLUT.$= Just (mouseCB ref)
  GLUT.passiveMotionCallback  GLUT.$= Just (mouseCB ref)
  GLUT.reshapeCallback        GLUT.$= Just (reshapeCB ref)
  withTimer (party ref >> step ref)
  where 
    keyboardMouseCB ref k ks mod pos =
      case (k,ks) of
        (GLUT.Char '\ESC', GLUT.Down) -> cleanup ref >> exitWith ExitSuccess
        (GLUT.Char 'r', GLUT.Down) -> reloadProgram opts ref
        _ -> return () -- putStrLn ("k: " ++ show k ++ ", ks: " ++ show ks)

    mouseCB ref (GLUT.Position x y) =
      modifyIORef ref (\s -> s {mousePosition = (fromIntegral x, fromIntegral y)})

cleanup :: PRef -> IO ()
cleanup ref = do
  state <- readIORef ref
  errorCheckValue <- GL.glGetError

  GL.glUseProgram 0
  GL.glDeleteProgram (programId state)

  GL.glDisableVertexAttribArray 0

  GL.glBindBuffer GL.gl_ARRAY_BUFFER 0
  withArrayLen [arrayBuffer state] $ GL.glDeleteBuffers . fromIntegral

  GL.glBindBuffer GL.gl_ELEMENT_ARRAY_BUFFER 0
  withArrayLen [elementBuffer state] $ GL.glDeleteBuffers . fromIntegral

  GL.glBindVertexArray 0
  withArrayLen [vaoId state] $
    GL.glDeleteVertexArrays . fromIntegral

  withArrayLen (map fst (textures state)) $
    GL.glDeleteTextures . fromIntegral

  errorCheckValue <- GL.glGetError
  when (errorCheckValue /= GL.gl_NO_ERROR) $ do
    putStrLn "ERROR: Could not destroy the GL objects"
    GLUT.reportErrors

size as = fromIntegral (length as * sizeOf (head as))

display :: PRef -> IO ()
display r = do
  state <- readIORef r
  let t = currentTime state
      p = programId state
  --GL.glBindVertexArray (vaoId state)
  case M.lookup "time" (uniforms state) of
    Nothing -> return ()
    Just loc -> GL.glUniform1f loc (realToFrac (t/1000))
  GL.glClear . sum . map fromIntegral $ [GL.gl_COLOR_BUFFER_BIT, GL.gl_DEPTH_BUFFER_BIT]
  GL.glDrawElements GL.gl_TRIANGLES 6 GL.gl_UNSIGNED_BYTE nullPtr
  GLUT.swapBuffers
  --GLUT.postRedisplay (Just (windowHandle state))

reloadProgram :: CmdLine -> PRef -> IO ()
reloadProgram opts ref = do
  state <- readIORef ref
  when (0 /= programId state) 
    (GL.glDeleteProgram . programId $ state)

  let path = ".":include opts
  p <- loadProgram path (vertFile state) (fragFile state)
  modifyIORef ref (\state -> state {programId = p})
  GL.glUseProgram p

  forM_ [0,1,2,3] $ \i -> do
    case M.lookup ("tex"++show i) (uniforms state) of
      Nothing -> return ()
      Just loc -> GL.glUniform1i loc i

  w <- currentWidth `fmap` readIORef ref
  h <- currentHeight `fmap` readIORef ref
  case M.lookup "resolution" (uniforms state) of
    Nothing -> return ()
    Just loc -> GL.glUniform2f loc (fromIntegral w) (fromIntegral h)

  print "reloadProgram : glBindVertexArray"

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

main :: IO ()
main = do
  c <- cmdLine
  win <- openWindow "pixelparty" (width c , height c)
  ref <- initGL c win
  simpleGLUTinit c display ref
  GLUT.mainLoop