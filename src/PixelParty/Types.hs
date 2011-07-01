{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  PixelParty.Types
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Used Types
--
--------------------------------------------------------------------------------

module PixelParty.Types
where

import System.Console.CmdArgs (Data, Typeable)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Map as M
import qualified Data.Time as T
import Control.Monad.State

type GLFragmentShader = GL.GLuint
type GLVertexShader = GL.GLuint
type GLProgram = GL.GLuint
type GLTextureUnit = GL.GLenum

newtype P a = P (StateT PartyState IO a)
  deriving (Functor, Monad, MonadIO, MonadState PartyState)

io :: MonadIO m => IO a -> m a
io = liftIO

runP :: PartyState -> P a -> IO (a, PartyState)
runP st (P a) = runStateT a st

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
  , currentWidth :: !Int -- unnecessary ?
  , currentHeight :: !Int -- unnecessary ?
  , vertFile :: FilePath -- unnecessary ?
  , fragFile :: FilePath -- unnecessary ?
  , startTime :: !T.UTCTime
  , done :: !Bool
  , frameCount :: !Int
  , fpsLastTime :: !T.UTCTime
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
  , currentWidth = 600
  , currentHeight = 600
  , vertFile = ""
  , fragFile = ""
  , startTime = T.UTCTime (T.ModifiedJulianDay 0) 0 -- 1858-11-17 00:00:00 UTC
  , done = False
  , frameCount = 0
  , fpsLastTime = T.UTCTime (T.ModifiedJulianDay 0) 0 -- 1858-11-17 00:00:00 UTC
  }

