module Types
where

import Data.IORef (IORef(..), newIORef, modifyIORef, readIORef)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Map as M
import qualified Graphics.UI.GLUT as GLUT

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
  , frameCount :: Int
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
  , frameCount = 0
  , currentWidth = 600
  , currentHeight = 600
  , windowHandle = undefined
  , vertFile = ""
  , fragFile = ""
  }

type WindowHandle = GLUT.Window

