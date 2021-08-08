module JavaScriptRunner where

import System.Process ( readProcess ) 

runJS :: FilePath -> String -> IO String 
runJS libPath code = do 
  lib <- readFile libPath
  ret <- readProcess "node" ["-"] (lib ++ "\r\n\r\n" ++ code ++ "\r\n\r\n" ++ "console.log(main);")
  return (init ret)