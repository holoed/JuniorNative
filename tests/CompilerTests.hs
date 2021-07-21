{-# LANGUAGE QuasiQuotes #-}
module CompilerTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( full )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)

build :: String -> IO String
build code = do
   (x, _, _) <- run (full code) (Interp.env, classEnv) (env, [])
   return $ either show id x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Compiler Tests" $ do

   it "value" $ "let main = 42" --> "[I 42]"

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "[I 6]"

   it "recursive function" $ [i|
      let fac n = if n == 0 then 1 else n * (fac (n - 1))
      let main = fac 5
   |] --> "[I 120]"
