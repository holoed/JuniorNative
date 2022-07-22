{-# LANGUAGE QuasiQuotes #-}
module UnitTests.OptimizeTypeClassesSpec where

import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import Junior.Compiler.Compiler ( closedAndANF, step )
import Junior.Compiler.CompilerMonad ( run, CompileM )
import Junior.Compiler.Intrinsics (env, classEnv)
import qualified Junior.Interpreter.InterpreterIntrinsics as Interp (env)
import Data.Text (unpack, Text)
import Data.String.Interpolate (i)
import Junior.Compiler.CompilerSteps (optimizeTypeClasses, deadCodeElimin, prettyPrintModule)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch (MonadThrow)

compile :: String -> CompileM Text
compile = closedAndANF >=>
       step "Optimize away fully resolved type classes instances" optimizeTypeClasses >=>
       step "Dead code elimination" deadCodeElimin >=>
       step "pretty print module" prettyPrintModule

build :: String -> IO String
build code = do
   (x, _, _) <- run (compile code) ("", Interp.env) (classEnv, env, [], [])
   either (return . show) (return . unpack) x

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) =>  String -> String -> m ()
(-->) s1 s2 = liftIO $ build s1 >>= (`shouldBe` s2)

tests :: TopSpec
tests = parallel $ do
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
let _f0 (_env, x0) = let anf_2 = 1 in
                     let anf_4 = AppClosure (nativeInt, anf_2) in
                     nativeAddInt x0 anf_4

val f :: Int -> Int
let f = let _c0 = MkClosure _f0 in
        SetEnv ("numInt", numInt, _c0)

val main :: Int
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeInt, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native subtraction inside a function" $ [i|
      val f :: Int -> Int
      let f x = x - 1
      let main = f 5
   |] --> [i|val _f0 :: Int -> Int
let _f0 (_env, x0) = let anf_2 = 1 in
                     let anf_4 = AppClosure (nativeInt, anf_2) in
                     nativeSubInt x0 anf_4

val f :: Int -> Int
let f = let _c0 = MkClosure _f0 in
        SetEnv ("numInt", numInt, _c0)

val main :: Int
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeInt, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native divide inside a function" $ [i|
      val f :: Double -> Double
      let f x = x / 2.5
      let main = f 5
   |] --> [i|val _f0 :: Double -> Double
let _f0 (_env, x0) = let anf_2 = 2.5 in
                     let anf_4 = AppClosure (nativeDouble, anf_2) in
                     nativeDivDouble x0 anf_4

val f :: Double -> Double
let f = let _c0 = MkClosure _f0 in
        SetEnv (\"fractionalDouble\", fractionalDouble, _c0)

val main :: Double
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeDouble, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native int divide inside a function" $ [i|
      val f :: Int -> Int
      let f x = x / 2
      let main = f 5
   |] --> [i|val _f0 :: Int -> Int
let _f0 (_env, x0) = let anf_2 = 2 in
                     let anf_4 = AppClosure (nativeInt, anf_2) in
                     nativeDivInt x0 anf_4

val f :: Int -> Int
let f = let _c0 = MkClosure _f0 in
        SetEnv (("fractionalInt", fractionalInt, SetEnv ("numInt", numInt, _c0)))

val main :: Int
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeInt, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native eq inside a function" $ [i|
      val f :: Double -> Bool
      let f x = x == 2.5
      let main = f 5
   |] --> [i|val _f0 :: Double -> Bool
let _f0 (_env, x0) = let anf_2 = 2.5 in
                     let anf_4 = AppClosure (nativeDouble, anf_2) in
                     nativeEqDouble x0 anf_4

val f :: Double -> Bool
let f = let _c0 = MkClosure _f0 in
        SetEnv (("eqDouble", eqDouble, SetEnv ("fractionalDouble", fractionalDouble, _c0)))

val main :: Bool
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeDouble, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native gt inside a function" $ [i|
      val f :: Double -> Bool
      let f x = x > 2.5
      let main = f 5
   |] --> [i|val _f0 :: Double -> Bool
let _f0 (_env, x0) = let anf_2 = 2.5 in
                     let anf_4 = AppClosure (nativeDouble, anf_2) in
                     nativeGtDouble x0 anf_4

val f :: Double -> Bool
let f = let _c0 = MkClosure _f0 in
        SetEnv (("fractionalDouble", fractionalDouble, SetEnv ("ordDouble", ordDouble, _c0)))

val main :: Bool
let main = let anf_6 = 5 in
           let anf_7 = AppClosure (nativeDouble, anf_6) in
           AppClosure (f, anf_7)
|]

   it "native lt inside a function" $ [i|
      val f :: Double -> Bool
      let f x = x > 2.5 || x < 3.5
      let main = f 5
   |] --> [i|val _f0 :: Double -> Bool
let _f0 (_env, x0) = let anf_2 = 2.5 in
                     let anf_4 = AppClosure (nativeDouble, anf_2) in
                     let anf_5 = nativeGtDouble x0 anf_4 in
                     let anf_11 = AppClosure (||, anf_5) in
                     let anf_8 = 3.5 in
                     let anf_10 = AppClosure (nativeDouble, anf_8) in
                     let anf_12 = nativeLtDouble x0 anf_10 in
                     AppClosure (anf_11, anf_12)

val f :: Double -> Bool
let f = let _c0 = MkClosure _f0 in
        SetEnv (("fractionalDouble", fractionalDouble, SetEnv ("ordDouble", ordDouble, _c0)))

val main :: Bool
let main = let anf_14 = 5 in
           let anf_15 = AppClosure (nativeDouble, anf_14) in
           AppClosure (f, anf_15)
|]