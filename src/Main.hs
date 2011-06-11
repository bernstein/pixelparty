module Main 
where

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
import PixelParty.Main

main :: IO ()
main = do
  opts <- cmdLine
  pixelparty opts
