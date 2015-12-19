-- -----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- pixelparty application
--
--------------------------------------------------------------------------------

module Main 
where

import PixelParty.CmdLine
import PixelParty.Main

main :: IO ()
main = do
  opts <- cmdLine
  pixelparty opts
