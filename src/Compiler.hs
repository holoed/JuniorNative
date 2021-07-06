module Compiler where

import TypedAst (TypedExp)
import SymbolTable (Symbol)
import StringUtils (padR)
import CompilerMonad ( CompileM )
import Control.Monad ( (>=>) )
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Writer( MonadWriter(tell) )
import CompilerSteps ( parse, fromSynExpToExp, dependencyAnalysis, typeInference, buildSymbolTable )
import System.TimeIt ( timeItT )
import Text.Printf ( printf )

step :: String -> (a -> CompileM b) -> (a -> CompileM b)
step desc f x = catchError 
                 (do (elapsedTime, ret) <- timeItT $ f x
                     tell [printf (padR 40 desc ++ "%6.2fs") elapsedTime]
                     return ret) (\e -> do tell [padR 40 desc ++ "failed"]
                                           throwError e )


pipeline :: String -> CompileM ([TypedExp], [Symbol])
pipeline = step "code to parse tree" parse >=>
           step "parse tree to syntax tree" fromSynExpToExp >=>
           step "dependency analysis" dependencyAnalysis >=>
           step "type inference" typeInference >=>
           step "build symbol table" buildSymbolTable

