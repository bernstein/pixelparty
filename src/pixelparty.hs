module Main where

-- TODO:
--   add ping-pong flag:
--     render to texture. supply that texture as an input for the next frame

import Control.Applicative ((<$>), pure)
import Control.Monad (unless, forM_)
import Control.Exception (bracket_)
import System.Directory (doesFileExist)
import System.Exit (exitWith,ExitCode(..))
import System (getArgs)
import System.Console.GetOpt
import Foreign (nullPtr, withArray)
import Foreign.Storable (Storable, sizeOf)
import Data.IORef (IORef(..), newIORef, modifyIORef, readIORef)
import Data.List (foldl')
import Data.Maybe (maybeToList)

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL (($=),($=!))
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GL3

import Data.Bitmap.OpenGL
import Codec.Image.STB

type PartyRef = IORef PartyState

data PartyState = PartyState {
    currentTime   :: Float
  , mousePosition :: (Double,Double)
  -- opengl stuff
  , program       :: Maybe GL.Program
  -- , uniforms :: M.Map String GL.UniformLocation
  , arrayBuffer   :: GL.BufferObject
  , elementBuffer :: GL.BufferObject
  -- , sampler
  , textures      :: [(GL.TextureObject,GL.TextureUnit)]
  , positions     :: GL.AttribLocation
  -- , test -- depth test
  -- , clear -- clearcolor
  -- , rasterizer -- viewport size, pos
  -- , draw
  }

defaultPartyState :: PartyState
defaultPartyState = PartyState
  { currentTime = 0
  , mousePosition = (0,0)
  , program = Nothing
  , arrayBuffer = GL.BufferObject 0
  , elementBuffer = GL.BufferObject 0
  , textures = []
  , positions = GL.AttribLocation 0
  }

programEd :: (Maybe GL.Program -> Maybe GL.Program) -> PartyState -> PartyState
programEd f s = s {program = f (program s)}

loadTexture :: FilePath -> GL.TextureUnit -> IO (GL.TextureObject, GL.TextureUnit)
loadTexture path unit = do 
  e  <- loadImage path
  case e of
    Left err -> error $ "loadTexture: " ++ err
    Right im -> do
      GL.activeTexture $= unit
      t <- makeSimpleBitmapTexture im
      return (t,unit)

compileShader :: GL.Shader s => String -> IO s
compileShader src = do
  [shaderId] <- GL.genObjectNames 1
  GL.shaderSource shaderId $= [src]
  GL.compileShader shaderId
  GLUT.reportErrors
  ok <- GL.get (GL.compileStatus shaderId)

  unless ok $ do
    infoLog <- GL.get (GL.shaderInfoLog shaderId)
    mapM_ putStrLn ["Shader info log :", infoLog, ""]
    GL.deleteObjectNames [shaderId]
    ioError $ userError "shader compilation failed"
  return shaderId

loadShaders :: [GL.VertexShader] -> [GL.FragmentShader] -> IO GL.Program
loadShaders vs fs = do
  [progId] <- GL.genObjectNames 1
  GL.attachedShaders progId $= (vs, fs)
  GL.linkProgram progId
  ok <- GL.get (GL.linkStatus progId)
  unless ok $ do
    GLUT.reportErrors
    infoLog <- GL.get (GL.programInfoLog progId)
    mapM_ putStrLn ["Program info log:", infoLog, ""]
    GL.deleteObjectNames [progId]
    ioError (userError "linking failed")
  return progId

reshapeCB :: PartyRef -> GL.Size -> IO ()
reshapeCB state s@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, s)
  prog <- GL.get GL.currentProgram
  case prog of
    Just p -> do
      resLoc <- GL.get (GL.uniformLocation p "resolution")
      GL.uniform resLoc $= GL.Vertex2 (fromIntegral w) (fromIntegral h :: GL.GLfloat)
    _ -> return ()
  GL.flush

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

loadProgram :: [String] -> FilePath -> FilePath -> IO GL.Program
loadProgram path vs fs = do
  v <- compileShader =<< includeFiles path =<< readFile vs
  f <- compileShader =<< includeFiles path =<< readFile fs
  loadShaders [v] [f]

quad :: [GL.GLfloat]
quad = [ -1.0, -1.0
       , -1.0,  1.0
       ,  1.0,  1.0
       ,  1.0, -1.0
       ]

quadIndices :: [GL.GLuint]
quadIndices = [0,1,2,0,2,3]

enableTexture :: (GL.TextureObject,GL.TextureUnit) -> IO ()
enableTexture (t,u) = do
  GL.activeTexture $= u
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just t

initGL :: Options -> GLUT.Window -> IO PartyRef
initGL opts win = do
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 0.0
  GL.depthFunc  $= Just GL.Less
  ab <- buffer GL.ArrayBuffer GL.StaticDraw quad
  eb <- buffer GL.ElementArrayBuffer GL.StaticDraw quadIndices 
  ts <- mapM (uncurry loadTexture) (zip (optTextures opts) $ map GL.TextureUnit [0..])
  mapM_ enableTexture ts
  ref <- newIORef defaultPartyState { arrayBuffer = ab 
                                    , elementBuffer = eb
                                    , textures = ts }
  reloadProgram opts ref
  return ref

openWindow :: String -> (Int,Int) -> IO GLUT.Window
openWindow title (w,h) = do
  GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered
                              , GLUT.RGBAMode
                              , GLUT.WithDepthBuffer
                              , GLUT.WithAlphaComponent ] 
  GLUT.initialWindowSize $= GL.Size (fromIntegral w) (fromIntegral h)
  GLUT.createWindow title

step :: PartyRef -> IO ()
step ref = modifyIORef ref (inc interval)
  where inc dt state = state { currentTime = currentTime state + fromIntegral dt}

withTimer :: IO () -> IO ()
withTimer io = GLUT.addTimerCallback interval (io >> withTimer io)

interval :: Int
interval = 10

simpleGLUTinit :: Options -> (PartyRef -> IO ()) -> PartyRef -> IO ()
simpleGLUTinit opts party ref = do
  GLUT.displayCallback        $= return ()
  GLUT.keyboardMouseCallback  $= Just (keyboardMouseCB ref)
  GLUT.motionCallback         $= Just (mouseCB ref)
  GLUT.passiveMotionCallback  $= Just (mouseCB ref)
  GLUT.reshapeCallback        $= Just (reshapeCB ref)
  withTimer (clear >> party ref >> swap >> step ref)
  where 
    keyboardMouseCB ref k ks mod pos =
      case (k,ks) of
        (GLUT.Char '\ESC', GLUT.Down) -> cleanup ref >> exitWith ExitSuccess
        (GLUT.Char 'r', GLUT.Down) -> reloadProgram opts ref
        _ -> return () -- putStrLn ("k: " ++ show k ++ ", ks: " ++ show ks)

    mouseCB ref (GL.Position x y) =
      modifyIORef ref (\s -> s {mousePosition = (fromIntegral x, fromIntegral y)})
    clear = GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    swap = GLUT.swapBuffers

cleanup :: PartyRef -> IO ()
cleanup ref = do
  state <- readIORef ref
  GL.deleteObjectNames . maybeToList . program $ state
  GL.deleteObjectNames . pure . arrayBuffer $ state
  GL.deleteObjectNames . pure . elementBuffer $ state
  mapM_ (GL.deleteObjectNames . pure . fst) $ textures state

buffer :: (Storable a) => GL.BufferTarget -> GL.BufferUsage -> [a] -> IO GL.BufferObject
buffer target usage xs = do 
  [ab] <- GL.genObjectNames 1
  GL.bindBuffer target GL.$= Just ab
  withArray xs $ \ptr -> GL.bufferData target $= (size xs, ptr, GL.StaticDraw)
  return ab
  where
    size as = fromIntegral (length as * sizeOf (head as))

display :: PartyRef -> IO ()
display ref = do
  state <- readIORef ref
  let t = currentTime state
      (Just p) = program state
  timeLoc <- GL.get $ GL.uniformLocation p "time"
  GL.uniform timeLoc $= GL.Index1 (realToFrac t :: GL.GLfloat)
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

reloadProgram :: Options -> PartyRef -> IO ()
reloadProgram opts ref = do
  GL.deleteObjectNames . maybeToList . program =<< readIORef ref

  let path = ".":optIPath opts
  p <- loadProgram path (optVert opts) (optFrag opts)
  modifyIORef ref (\state -> state {program = Just p})
  GL.currentProgram $= Just p

  forM_ [0,1,2,3] $ \i -> do
    loc <- GL.get (GL.uniformLocation p ("tex"++show i))
    GL.uniform loc $= GL.Index1 (fromIntegral i :: GL.GLint)

  resLoc <- GL.get (GL.uniformLocation p "resolution")
  (_, GL.Size w h) <- GL.get GL.viewport
  GL.uniform resLoc $= GL.Vertex2 (fromIntegral w) (fromIntegral h ::GL.GLfloat)

  pos <- GL.get (GL.attribLocation p "position")
  GL.vertexAttribArray pos $= GL.Enabled
  modifyIORef ref (\state -> state {positions = pos})

  (GL.bindBuffer GL.ArrayBuffer GL.$=) . pure . arrayBuffer =<< readIORef ref

  let storageSize = 2
  GL.vertexAttribPointer pos $= 
    (GL.ToFloat, GL.VertexArrayDescriptor storageSize GL.Float 0 nullPtr)

  (GL.bindBuffer GL.ElementArrayBuffer GL.$=).pure.elementBuffer =<< readIORef ref

options :: [OptDescr (Options -> Options)]
options = 
  [ Option "V" ["version"] 
    (NoArg (\opts -> opts { optShowVersion = True }))   "show version number"
  , Option "t" ["tex"]   
    (ReqArg (\f opts -> opts { optTextures = optTextures opts ++ [f]}) "FILE") "texture name"
  , Option "f" ["fragment"]   
    (ReqArg (\f opts -> opts { optFrag = f}) "FILE") "fragment shader file name"
  , Option "v" ["vertex"]   
    (ReqArg (\v opts -> opts { optVert = v}) "FILE") "vertex shader file name"
  , Option "I" ["include"]   
    (ReqArg (\i opts -> opts { optIPath = optIPath opts ++ [i]}) "FILE") "shader include path"
  , Option "w" ["width"]
    (ReqArg (\w opts -> opts { optWidth = read w}) "") "window width"
  , Option "h" ["height"]
    (ReqArg (\h opts -> opts { optHeight = read h}) "") "window height"
  ]

data Options = Options
  { optShowVersion  :: Bool
  , optVert         :: FilePath
  , optFrag         :: FilePath
  , optTextures     :: [FilePath]
  , optIPath        :: [FilePath]
  , optWidth        :: Int
  , optHeight       :: Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optShowVersion = False
  , optVert = "vsfs.vert"
  , optFrag = "vsfs.frag"
  , optTextures = []
  , optIPath = ["."]
  , optWidth = 800
  , optHeight = 600
  }

processOptions :: [String] -> IO (Options, [String])
processOptions argv = case getOpt RequireOrder options argv of
    (o, n, []) -> return (foldl' (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: pixelparty [OPTION...]"

main :: IO ()
main = do
  (opts,s) <- processOptions =<< getArgs
  simpleGLUTinit opts display =<< initGL opts =<< openWindow "pixelparty" (optWidth opts, optHeight opts)
  GLUT.mainLoop
