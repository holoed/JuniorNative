{-# LANGUAGE QuasiQuotes #-}
module OptimizeTypeClassesTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( closedAndANF, step )
import CompilerMonad ( run, CompileM )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack, Text)
import Data.String.Interpolate (i)
import CompilerSteps (optimizeTypeClasses, deadCodeElimin, prettyPrintModule)
import Control.Monad ((>=>))

compile :: String -> CompileM Text
compile = closedAndANF >=>
       step "Optimize away fully resolved type classes instances" optimizeTypeClasses >=>
       step "Dead code elimination" deadCodeElimin >=>
       step "pretty print module" prettyPrintModule

build :: String -> IO String
build code = do
   (x, _, _) <- run (compile code) (Interp.env, classEnv) (env, [])
   either (return . show) (return . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Optimize resolved Type Classes Known Instances" $ do

   it "native eq" $ "let main = 2 == 3" --> [i|val main :: Bool
let main = 
    let anf_0 = AppClosure (fromInteger, numInt) in
    let anf_1 = 2 in
    let anf_3 = AppClosure (anf_0, anf_1) in
    let anf_4 = AppClosure (fromInteger, numInt) in
    let anf_5 = 3 in
    let anf_7 = AppClosure (anf_4, anf_5) in
    nativeEqInt (anf_3, anf_7)
|]

   it "native plus" $ "let main = 2 + 3" --> [i|val main :: Int
let main = 
    let anf_0 = AppClosure (fromInteger, numInt) in
    let anf_1 = 2 in
    let anf_3 = AppClosure (anf_0, anf_1) in
    let anf_4 = AppClosure (fromInteger, numInt) in
    let anf_5 = 3 in
    let anf_7 = AppClosure (anf_4, anf_5) in
    nativeAddInt (anf_3, anf_7)
|]