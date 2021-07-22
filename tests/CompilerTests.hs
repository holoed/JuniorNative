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

   it "recursive function 2" $ [i|
      let fib n = if n == 0 then 0 else 
                  if n == 1 then 1 else 
                  fib (n - 1) + fib (n - 2)
      let main = fib 10
   |] --> "[I 55]"

   it "depending on a top level value affected by monomorphic restriction" $ [i|
      let x = 42
      let main = x + 1
   |] --> "[I 43]"

   it "higher order recursive function" $ [i|
      let foldr f v xs =
      if (null xs) then v
      else f (head xs) (foldr f v (tail xs))
      
      let xs = 1:2:3:4:5:[]

      let main = foldr (*) 1 xs 
   |] --> "[I 120]"
