module OptimizeTypeClassesTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( closedAndANF, step )
import CompilerMonad ( run, CompileM )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack, Text)
import CompilerSteps (optimizeTypeClasses, toJs)
import Control.Monad ((>=>))

compile :: String -> CompileM Text
compile = closedAndANF >=>
       step "Optimize away fully resolved type classes instances" optimizeTypeClasses >=>
       step "to javascript" toJs

build :: String -> IO String
build code = do
   (x, _, _) <- run (compile code) (Interp.env, classEnv) (env, [])
   either (return . show) (return . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Optimize resolved Type Classes Known Instances" $ do

   it "value" $ "let main = 2 == 3" --> "const main = function () { const anf_2 = nativeEqInt; const anf_0 = applyClosure(fromInteger,numInt); const anf_1 = 2; const anf_3 = applyClosure(anf_0,anf_1); const anf_6 = applyClosure(nativeEqInt,anf_3); const anf_4 = applyClosure(fromInteger,numInt); const anf_5 = 3; const anf_7 = applyClosure(anf_4,anf_5); return (nativeEqInt ([anf_3,anf_7])) }();"