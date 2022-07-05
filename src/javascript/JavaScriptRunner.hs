module JavaScriptRunner where

import System.Process ( readProcess ) 

runJS :: FilePath -> String -> IO String 
runJS libPath code = do 
  lib <- readFile libPath
  let js = lib  ++ "\r\n\r\n" ++
           "try {" ++ "\r\n\r\n" ++ 
           code ++ "\r\n\r\n" ++ 
           "if (main instanceof Promise) {" ++
           "main.then(x => console.log(JSON.stringify(x))) " ++
           "} else " ++  
           "console.log(JSON.stringify(main));"
           ++ "\r\n\r\n" ++
           "} catch (e) { console.log(e); }"
 -- putStrLn js
  ret <- readProcess "node" ["-"] js
  return (init ret)