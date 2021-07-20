{-# LANGUAGE QuasiQuotes #-}
module CompilerTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( full )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)

build :: String -> IO String
build code = do
   (Right x, _, _) <- run (full code) classEnv (env, [])
   return x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Compiler Tests" $ do

   it "value" $ "let x = 42" --> [i|val x :: Int
let x = fromInteger numInt 42
|]

   it "function" $ "let f x = x + 1" --> [i|val f :: Num a -> a -> a
let f numT20 x = 
    (numT20 + x) (fromInteger numT20 1)
|]
