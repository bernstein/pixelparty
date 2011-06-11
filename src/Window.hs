module Window
where

import Types
import qualified Graphics.UI.SDL as SDL

openWindow' :: String -> (Int,Int) -> IO SDL.Surface
openWindow' title (w,h) = do
  SDL.init [SDL.InitVideo]
  s <- SDL.setVideoMode w h 32 [SDL.OpenGL, SDL.Resizable, SDL.DoubleBuf]
  SDL.glSetAttribute SDL.glDoubleBuffer 1
  return s

swapBuffers = SDL.glSwapBuffers
