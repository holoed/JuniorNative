module Compiler where

import StringUtils (padR)
import CompilerMonad ( CompileM )
import Control.Monad ( (>=>) )
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Writer( MonadWriter(tell) )
import CompilerSteps ( parse, fromSynExpToExp, dependencyAnalysis, typeInference, buildSymbolTable, prettyPrintModule, applyMonomorphicRestriction, desugarPredicates )
import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import TypedAst (TypedExp)

step :: String -> (a -> CompileM b) -> (a -> CompileM b)
step desc f x = catchError 
                 (do (elapsedTime, ret) <- timeItT $ f x
                     tell [printf (padR 40 desc ++ "%6.2fs") elapsedTime]
                     return ret) (\e -> do tell [padR 40 desc ++ "failed"]
                                           throwError e )


frontEnd :: String -> CompileM [TypedExp]
frontEnd = step "code to parse tree" parse >=>
           step "parse tree to syntax tree" fromSynExpToExp >=>
           step "dependency analysis" dependencyAnalysis >=>
           step "type inference" typeInference >=>
           step "build symbol table" buildSymbolTable 

frontEndPrinted :: String -> CompileM String
frontEndPrinted = frontEnd >=> 
                  step "pretty print module" prettyPrintModule

full :: String -> CompileM String
full = frontEnd >=>
       step "apply monomorphic restriction" applyMonomorphicRestriction >=>
       step "desugar constraints" desugarPredicates >=>
       step "pretty print module" prettyPrintModule
       