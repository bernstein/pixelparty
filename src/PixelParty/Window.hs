-- -----------------------------------------------------------------------------
-- |
-- Module      :  PixelParty.Window
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Open opengl window.
--
--------------------------------------------------------------------------------

module PixelParty.Window
  ( 
    openWindow
  -- , resizeWindow
  -- , swapBuffers
  ) where

import PixelParty.Types
import qualified Graphics.UI.SDL as SDL
import Foreign ((.|.))
import Foreign.C

--openWindow :: String -> (Int,Int) -> IO SDL.Surface
openWindow title (w,h) = do
  SDL.init SDL.SDL_INIT_EVERYTHING -- >>= SDL.err
  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 3
  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 3
  SDL.glSetAttribute SDL.SDL_GL_CONTEXT_PROFILE_MASK SDL.SDL_GL_CONTEXT_PROFILE_CORE
  SDL.glSetAttribute SDL.SDL_GL_RED_SIZE 5
  SDL.glSetAttribute SDL.SDL_GL_GREEN_SIZE 5
  SDL.glSetAttribute SDL.SDL_GL_BLUE_SIZE 5
  SDL.glSetAttribute SDL.SDL_GL_DEPTH_SIZE 16
  SDL.glSetAttribute SDL.SDL_GL_DOUBLEBUFFER 1

  let flags = SDL.SDL_WINDOW_OPENGL
        .|. SDL.SDL_WINDOW_SHOWN
        -- .|. SDL_WINDOW_RESIZABLE
        -- .|. SDL.SDL_WINDOW_FULLSCREEN_DESKTOP

  -- s <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
  -- SDL_GL_DOUBLEBUFFER
  -- SDL.glSetAttribute SDL.glDoubleBuffer 1
  -- SDL.setCaption title title

  window <- withCString title $ \t -> SDL.createWindow t
              SDL.SDL_WINDOWPOS_CENTERED
              SDL.SDL_WINDOWPOS_CENTERED
              (fromIntegral w) (fromIntegral h) flags -- >>= SDL.errOnNull
  ctx <- SDL.glCreateContext window -- >>= SDL.errOnNull
  SDL.glMakeCurrent window ctx
  return window

--resizeWindow :: Int -> Int -> IO SDL.Surface
-- resizeWindow w h = SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]

--swapBuffers = SDL.glSwapWindow
