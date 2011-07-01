-- -----------------------------------------------------------------------------
-- |
-- Module      :  PixelParty.CmdLine
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Command line options.
--
--------------------------------------------------------------------------------

module PixelParty.CmdLine
  ( CmdLine(..)
  , cmdLine
  ) where

import PixelParty.Types
import System.Console.CmdArgs

showVersion = "1.0.0"

frag = Fragment
  { fshader = def &= args &= typ "FRAGMENTSHADER"
  , vshader = def &= help "Vertex Shader" &= typ "VERTEXSHADER"
  , width = 600 &= typ "WIDTH"
  , height = 600 &= typ "HEIGHT"
  , include = def &= help "Include Path" &= typDir
  , tex = def &= help "Textures" &= typ "IMAGE" &= name "tex"
  } &= summary ("pixelparty v" ++ showVersion ++ ", (C) Andreas-C. Bernstein 2010-2011\n") &= program "pixelparty"

cmdLine :: IO CmdLine
cmdLine = cmdArgs frag
