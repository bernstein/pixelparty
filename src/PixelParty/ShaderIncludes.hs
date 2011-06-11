module PixelParty.ShaderIncludes 
  (
    includeFiles
  ) where

import System.Directory (doesFileExist)
import Control.Applicative ((<$>), pure)

includeFiles :: [String] -> String -> IO String
includeFiles path = pp path . lines

-- inspired by cpphs
pp :: [String] -> [String] -> IO String
pp _ [] = return []
pp path (l@('#':x):xs) = 
  let ws = words x
      cmd = head ws
      line = tail ws
      file ('"':ns) = init ns
      file ('<':ns) = init ns
      file _ = error "unknown include file"
  in  case cmd of
    "include" -> do (content) <- readFirstFound (file (unwords line)) path
                    pp path (lines content ++ xs)
    _ -> ((l++"\n")++) <$> pp path xs
pp path (x:xs) = ((x++"\n")++) <$> pp path xs

readFirstFound :: String -> [String] -> IO String
readFirstFound name [] = error $ "Warning: Can't find file " ++ name
readFirstFound name (p:ps) = do 
  let file = p ++ '/':name
  b <- doesFileExist file
  if b then readFile file else readFirstFound name ps 


