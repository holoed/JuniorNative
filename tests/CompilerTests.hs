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

   it "value" $ "let main = 42" --> "42"

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "6"

   it "recursive function" $ [i|
      let fac n = if n == 0 then 1 else n * (fac (n - 1))
      let main = fac 5
   |] --> "120"

   it "recursive function 2" $ [i|
      let fib n = if n == 0 then 0 else 
                  if n == 1 then 1 else 
                  fib (n - 1) + fib (n - 2)
      let main = fib 10
   |] --> "55"

   it "depending on a top level value affected by monomorphic restriction" $ [i|
      let x = 42
      let main = x + 1
   |] --> "43"

   it "higher order recursive function" $ [i|
      let foldr f v xs =
      if (null xs) then v
      else f (head xs) (foldr f v (tail xs))
      
      let xs = 1:2:3:4:5:[]

      let main = foldr (*) 1 xs 
   |] --> "120"

   it "higher order recursive function 2" $ [i|
      let foldl f v xs =
          if (null xs) then v
          else foldl f (f v (head xs)) (tail xs)
      
      let xs = 1:2:3:4:5:[]

      let main = foldl (*) 1 xs 
   |] --> "120"

   it "Predicates Resolution 1" $ [i|
   let foldr f v xs =
      if (null xs) then v
      else f (head xs) (foldr f v (tail xs))

   let (++) xs ys = foldr (:) ys xs 

   let main = pure 42 ++ []
   |] --> "[42]"

   it "Predicates Resolution 2" $ [i|
   let foldl f v xs =
       if (null xs) then v
       else foldl f (f v (head xs)) (tail xs)

   let foldr f v xs =
       if (null xs) then v
       else f (head xs) (foldr f v (tail xs))

   let mapM f as = 
       let k a r = bind (f a) (\\x ->
                   bind r     (\\xs -> 
                   pure (x:xs))) in   
       foldr k (pure []) as
      
   let main = mapM (\\x -> x:[]) (1:2:3:4:[])
   |] --> "[[1,2,3,4]]"
