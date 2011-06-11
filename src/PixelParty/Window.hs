module PixelParty.Window
  ( 
    openWindow
  , resizeWindow
  , swapBuffers
  ) where

import PixelParty.Types
import qualified Graphics.UI.SDL as SDL

openWindow :: String -> (Int,Int) -> IO SDL.Surface
openWindow title (w,h) = do
  SDL.init [SDL.InitVideo]
  s <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
  SDL.glSetAttribute SDL.glDoubleBuffer 1
  SDL.setCaption title title
  return s

resizeWindow :: Int -> Int -> IO SDL.Surface
resizeWindow w h = SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]

swapBuffers = SDL.glSwapBuffers