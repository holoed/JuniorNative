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
let main = let anf_1 = 2 in
           let anf_3 = AppClosure (nativeInt, anf_1) in
           let anf_5 = 3 in
           let anf_7 = AppClosure (nativeInt, anf_5) in
           nativeEqInt anf_3 anf_7
|]

   it "native plus" $ "let main = 2 + 3" --> [i|val main :: Int
let main = let anf_1 = 2 in
           let anf_3 = AppClosure (nativeInt, anf_1) in
           let anf_5 = 3 in
           let anf_7 = AppClosure (nativeInt, anf_5) in
           nativeAddInt anf_3 anf_7
|]

   it "native plus inside a function" $ [i|
      val f :: Int -> Int
      let f x = x + 1
      let main = f 5
   |] --> [i|val _f0 :: Int -> Int
let _f0 (_env, x) = let anf_2 = 1 in
                    let anf_4 = AppClosure (nativeInt, anf_2) in
                    nativeAddInt x anf_4

val f :: Int -> Int
let f = let _c0 = MkClosure _f0 in
        SetEnv ("numInt", numInt, _c0)

val main :: Int
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeInt, anf_6) in
           AppClosure (f, anf_7)
|]