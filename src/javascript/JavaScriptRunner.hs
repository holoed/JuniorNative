module JavaScriptRunner where

import System.Process ( readProcess ) 

runJS :: FilePath -> String -> IO String 
runJS libPath code = do 
  lib <- readFile libPath
  let js = lib  ++ "\r\n\r\n" ++ 
           code ++ "\r\n\r\n" ++ 
           "console.log(JSON.stringify(main));"
 -- putStrLn js
  ret <- readProcess "node" ["-"] js
  return (init ret)