module Compiler where

import CompilerMonad ( CompileM )
import Control.Monad ( (>=>) )
import Control.Monad.Writer( MonadWriter(tell) )
import CompilerSteps ( parse, fromSynExpToExp, dependencyAnalysis, typeInference, fromEnvToTypeDict )
import System.TimeIt ( timeItT )
import Text.Printf ( printf )

padR :: Int -> String -> String
padR n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s

step :: String -> (a -> CompileM b) -> (a -> CompileM b)
step desc f x =  do (elapsedTime, ret) <- timeItT $ f x
                    tell [printf (padR 40 desc ++ "%6.2fs") elapsedTime]
                    return ret


pipeline :: String -> CompileM [(String, String)]
pipeline = step "code to parse tree" parse >=>
           step "parse tree to syntax tree" fromSynExpToExp >=>
           step "dependency analysis" dependencyAnalysis >=>
           step "type inference" typeInference >=>
           step "to final view" fromEnvToTypeDict

