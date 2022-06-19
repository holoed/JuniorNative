{-# LANGUAGE QuasiQuotes #-}
module OptimizeClosureEnvsSpec where

import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import Compiler ( closedAndANF, step )
import CompilerMonad ( run, CompileM )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack, Text)
import Data.String.Interpolate (i)
import CompilerSteps (optimizeClosureEnvs, prettyPrintModule, optimizeTypeClasses, deadCodeElimin)
import Control.Monad ((>=>))

compile :: String -> CompileM Text
compile = closedAndANF >=>
       step "Optimize away fully resolved type classes instances" optimizeTypeClasses >=>
       step "Dead code elimination" deadCodeElimin >=>
       step "Optimize away not used SetEnvs" optimizeClosureEnvs >=>
       step "pretty print module" prettyPrintModule

build :: String -> IO String
build code = do
   (x, _, _) <- run (compile code) ("", Interp.env) (classEnv, env, [], [])
   either (return . show) (return . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

spec :: Spec
spec = parallel $ do
  describe "Optimize away not used setEnvs" $ do

   it "Remove unused SetEnv numInt" $ [i|
      val f :: Int -> Int
      let f x = x + 1
      let main = f 5
   |] --> [i|val _f0 :: Int -> Int
let _f0 (_env, x) = let anf_2 = 1 in
                    let anf_4 = AppClosure (nativeInt, anf_2) in
                    nativeAddInt x anf_4

val f :: Int -> Int
let f = let _c0 = MkClosure _f0 in _c0

val main :: Int
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeInt, anf_6) in
           AppClosure (f, anf_7)
|]

  it "Remove unused SetEnv in cases with Tuple pattern in let" $ [i|
val add :: ((Int, Int), (Int, Int)) -> (Int, Int)
let add (z1, z2) = 
  let (a, b) = z1 in
  let (c, d) = z2 in
  (a + c, b + d)
  
let main = add ((2, 3), (4, 5)) |] --> [i|val _f0 :: ((Int, Int), (Int, Int)) -> (Int, Int)
let _f0 (_env, (z1, z2)) = 
    let (a, b) = z1 in
    let (c, d) = z2 in
    let anf_5 = nativeAddInt b d in
    let anf_4 = nativeAddInt a c in
    (anf_4, anf_5)

val add :: ((Int, Int), (Int, Int)) -> (Int, Int)
let add = let _c0 = MkClosure _f0 in _c0

val main :: (Int, Int)
let main = let anf_15 = 5 in
           let anf_17 = AppClosure (nativeInt, anf_15) in
           let anf_13 = 4 in
           let anf_16 = AppClosure (nativeInt, anf_13) in
           let anf_19 = (anf_16, anf_17) in
           let anf_9 = 3 in
           let anf_11 = AppClosure (nativeInt, anf_9) in
           let anf_7 = 2 in
           let anf_10 = AppClosure (nativeInt, anf_7) in
           let anf_18 = (anf_10, anf_11) in
           let anf_20 = (anf_18, anf_19) in
           AppClosure (add, anf_20)
|] 