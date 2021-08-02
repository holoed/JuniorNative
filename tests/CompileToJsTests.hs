{-# LANGUAGE QuasiQuotes #-}
module CompileToJsTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( fullJS )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)

build :: String -> IO String
build code = do
   (x, _, _) <- run (fullJS code) (Interp.env, classEnv) (env, [])
   return $ either show unpack x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Compile to JavaScript Tests" $ do

   it "value" $ "let main = 42" --> 
         "var main = ((fromInteger (numInt)) (42))"

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> 
    [i|var f = function (numT2) {  return function (x) {  return (((__add (numT2)) (x)) (((fromInteger (numT2)) (1)))) } }
var main = ((f (numInt)) (((fromInteger (numInt)) (5))))|]