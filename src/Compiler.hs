module Compiler where

import Ast (Exp)
import Environment (Env)
import CompilerMonad ( CompileM )
import Control.Monad ( (>=>) )
import Control.Monad.Writer( MonadWriter(tell) )
import CompilerSteps ( parse, fromSynExpToExp, dependencyAnalysis, typeInference, fromEnvToTypeDict ) 

step :: String -> (a -> CompileM b) -> (a -> CompileM b)
step desc f x =  do tell [desc]
                    f x

pipeline :: String -> CompileM [(String, String)]
pipeline = step "code to parse tree" parse >=>
           step "parse tree to syntax tree" fromSynExpToExp >=>
           step "dependency analysis" dependencyAnalysis >=>
           step "type inference" typeInference >=>
           step "to final view" fromEnvToTypeDict 

