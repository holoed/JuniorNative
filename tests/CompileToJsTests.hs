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

   it "factorial example" $ [i|
     let fac n = if n == 0 then 1 else n * fac (n - 1)
     let main = fac 5
   |] --> "var fac = function (eqT14) {  return function (numT14) {  return function (n) {  return function() { if ((((__eqeq (eqT14)) (n)) (((fromInteger (numT14)) (0))))) { return ((fromInteger (numT14)) (1)) } else { return (((__mul (numT14)) (n)) ((((fac (eqT14)) (numT14)) ((((__sub (numT14)) (n)) (((fromInteger (numT14)) (1)))))))) } }() } } }\nvar main = (((fac (eqInt)) (numInt)) (((fromInteger (numInt)) (5))))"

   it "factorial with lists" $ [i|
   let foldl f v xs =
      if (null xs) then v
      else foldl f (f v (head xs)) (tail xs)
      
   let range startIndex endIndex =
      let range' acc endIndex =
            if startIndex > endIndex then acc
               else range' (endIndex : acc) (endIndex - 1) in
            range' [] endIndex
         
   let fac n = foldl (*) 1 (range 1 n)    
         
   let main = fac 5     
   |] --> "var foldl = function (f) {  return function (v) {  return function (xs) {  return function() { if ((isEmpty (xs))) { return v } else { return (((foldl (f)) (((f (v)) ((head (xs)))))) ((tail (xs)))) } }() } } }\nvar range = function (numT24) {  return function (ordT24) {  return function (startIndex) {  return function (endIndex) { var rangeQuoted = function (acc) {  return function (endIndex) {  return function() { if ((((__gt (ordT24)) (startIndex)) (endIndex))) { return acc } else { return ((rangeQuoted (((__colon (endIndex)) (acc)))) ((((__sub (numT24)) (endIndex)) (((fromInteger (numT24)) (1)))))) } }() } }; return ((rangeQuoted ([])) (endIndex)) } } } }\nvar fac = function (numT13T19T6) {  return function (ordT13T19T6) {  return function (n) {  return (((foldl ((__mul (numT13T19T6)))) (((fromInteger (numT13T19T6)) (1)))) (((((range (numT13T19T6)) (ordT13T19T6)) (((fromInteger (numT13T19T6)) (1)))) (n)))) } } }\nvar main = (((fac (numInt)) (ordInt)) (((fromInteger (numInt)) (5))))"

   it "deconstucting tuples" $ [i|
      let f (x, y) = x + y
      let main = f (2, 3)
   |] --> "var f = function (numT3) {  return function ([x,y]) {  return (((__add (numT3)) (x)) (y)) } }\nvar main = ((f (numInt)) ([((fromInteger (numInt)) (2)),((fromInteger (numInt)) (3))]))"