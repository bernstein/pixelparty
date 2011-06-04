{-# LANGUAGE DeriveDataTypeable #-}
module CmdLine
  ( CmdLine(..)
  , cmdLine
  ) where

import System.Console.CmdArgs

showVersion = "1.0.0"
data CmdLine = Fragment 
  { fshader :: FilePath
  , vshader :: FilePath
  , width :: Int
  , height :: Int
  , include :: [String]
  , tex :: [FilePath]
  } deriving (Show, Data, Typeable)

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
