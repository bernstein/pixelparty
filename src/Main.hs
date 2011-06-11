module Main 
where

import PixelParty.CmdLine
import PixelParty.Main

main :: IO ()
main = do
  opts <- cmdLine
  pixelparty opts
