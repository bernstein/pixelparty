{-# LANGUAGE DeriveDataTypeable #-}
module Types
where

import System.Console.CmdArgs (Data, Typeable)
import Data.IORef (IORef(..), newIORef, modifyIORef, readIORef)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Map as M
import qualified Graphics.UI.GLUT as GLUT
import qualified Data.Time as T

type GLFragmentShader = GL.GLuint
type GLVertexShader = GL.GLuint
type GLProgram = GL.GLuint
type GLTextureUnit = GL.GLenum

--type ErrorIO a = ErrorT String IO a

type PRef = IORef PartyState
-- notes: type P a = ReaderT (IORef PartyState) IO a
-- or: 
-- newtype P a = P (ReaderT (IORef PartyState) IO a) 
--   deriving (Functor, Monad, MonadIO, MonadReader (IORef PartyState))
--   modify :: (PartyState -> PartyState) -> P ()

data CmdLine = Fragment 
  { fshader :: FilePath
  , vshader :: FilePath
  , width :: Int
  , height :: Int
  , include :: [String]
  , tex :: [FilePath]
  } deriving (Show, Data, Typeable)

data PartyState = PartyState {
    currentTime   :: !Float

  -- opengl stuff
  , vertexShaderId :: !GL.GLuint
  , fragmentShaderId :: !GL.GLuint
  , programId       :: !GL.GLuint
  , vaoId :: !GL.GLuint
  , arrayBuffer   :: !GL.GLuint
  , elementBuffer :: !GL.GLuint
  , uniforms :: M.Map String GL.GLint
  -- , sampler
  , textures :: [(GL.GLuint,GL.GLenum)]
  , depthTest :: GL.GLenum
  -- , rasterizer -- viewport size, pos
  -- , draw
  , frameCount :: !Int
  , currentWidth :: !Int -- unnecessary ?
  , currentHeight :: !Int -- unnecessary ?
  , windowHandle :: WindowHandle
  , vertFile :: FilePath -- unnecessary ?
  , fragFile :: FilePath -- unnecessary ?
  , startTime :: !T.UTCTime
  }

defaultPartyState :: PartyState
defaultPartyState = PartyState
  { currentTime = 0
  , vertexShaderId = 0
  , fragmentShaderId = 0
  , programId = 0
  , vaoId = 0
  , arrayBuffer = 0
  , elementBuffer = 0
  , uniforms = M.empty
  , textures = []
  , depthTest = GL.gl_LESS
  , frameCount = 0
  , currentWidth = 600
  , currentHeight = 600
  , windowHandle = undefined
  , vertFile = ""
  , fragFile = ""
  , startTime = T.UTCTime (T.ModifiedJulianDay 0) 0 -- 1858-11-17 00:00:00 UTC
  }

type WindowHandle = GLUT.Window

