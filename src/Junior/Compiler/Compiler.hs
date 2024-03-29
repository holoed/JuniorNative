module Junior.Compiler.Compiler where

import Junior.Utils.StringUtils (padR)
import Junior.Compiler.CompilerMonad ( CompileM )
import Control.Monad ( (>=>) )
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Writer( MonadWriter(tell) )
import Junior.Compiler.CompilerSteps ( parse, fromSynExpToExp, dependencyAnalysis, typeInference, buildSymbolTable, prettyPrintModule, desugarPredicates, desugarRemote, desugarPatternMatching, interpret, toJs, closureConversion, aNormalisation, optimizeTypeClasses, deadCodeElimin, optimizeClosureEnvs, fromSynExpToDataDecl, compilePatternMatching, renameVars )
import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Interpreter.InterpreterMonad (showResult)
import Data.Text (Text, unwords)
import Prelude hiding (unwords)

step :: String -> (a -> CompileM b) -> (a -> CompileM b)
step desc f x = catchError
                 (do (elapsedTime, ret) <- timeItT $ f x
                     tell [printf (padR 40 desc ++ "%6.2fs") elapsedTime]
                     return ret) (\e -> do tell [padR 40 desc ++ " " ++ show e]
                                           throwError e )


frontEnd :: String -> CompileM [TypedExp]
frontEnd = step "code to parse tree" parse >=>
           step "extract data decls and update env" fromSynExpToDataDecl >=>
           step "parse tree to syntax tree" fromSynExpToExp >=>
           step "dependency analysis" dependencyAnalysis >=>
           step "type inference" typeInference >=>
           step "build symbol table" buildSymbolTable >=>
           step "alpha renamer" renameVars 

frontEndPrinted :: String -> CompileM Text
frontEndPrinted = frontEnd >=>
                  step "pretty print module" prettyPrintModule

backendPrinted :: String -> CompileM Text
backendPrinted = frontEnd >=>
       step "desugar pattern matching" desugarPatternMatching >=>
       step "compile pattern matching" compilePatternMatching >=>
       step "desugar constraints" desugarPredicates >=>
       step "pretty print module" prettyPrintModule

closedAndANF :: String -> CompileM [TypedExp]
closedAndANF = frontEnd >=>
       step "desugar pattern matching" desugarPatternMatching >=>
       step "compile pattern matching" compilePatternMatching >=>
       step "desugar constraints" desugarPredicates >=>
       step "A Normalisation" aNormalisation >=>
       step "closure conversion" closureConversion 
       
fullInterp :: String -> CompileM Text
fullInterp = frontEnd >=>
       step "desugar constraints" desugarPredicates >=>
       step "interpret" interpret >=>
       step "print" (return . unwords . (showResult <$>))

fullJS :: String -> CompileM Text
fullJS = frontEnd >=>
       step "desugar constraints" desugarPredicates >=>
       step "to javascript" toJs

fullJSClosedANF :: String -> CompileM Text
fullJSClosedANF = closedAndANF >=>
       step "Optimize resolved instances" optimizeTypeClasses >=>
       step "Dead code elimination" deadCodeElimin >=>
       step "Optimize away not used SetEnvs" optimizeClosureEnvs >=>
       step "desugar remote" desugarRemote >=>
       step "to javascript" toJs
       