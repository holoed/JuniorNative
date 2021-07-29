{-# LANGUAGE QuasiQuotes #-}
module ConstraintsResolutionTests where

import Types ( Pred(IsIn), Type(TyApp, TyVar, TyCon) ) 
import ConstraintsResolution (typeForPred, toCamel, varNameForPred)
import Test.Hspec ( it, describe, shouldBe, SpecWith, Expectation )
import TypesPrinter () 
import Intrinsics ( classEnv, env )
import Data.String.Interpolate ( i )
import Compiler ( backendPrinted )
import CompilerMonad ( run )
import InterpreterMonad (empty)
import Data.Text (unpack)

build :: String -> IO String
build code = do
   (x, _, _) <- run (backendPrinted code) (empty, classEnv) (env, [])
   return $ either show unpack x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do

  describe "Constraints Resolution Tests" $ do

   it "Type for a Predicate" $ do
       let (--->) x y = (show . typeForPred) x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) ---> "Num a"
       IsIn "Monad" (TyVar "m" 1) ---> "Monad m"
       IsIn "Num" (TyCon "Int") ---> "Num Int"
       IsIn "Reader" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) ---> "Reader (a, b)"

   it "camel case names" $ do
       toCamel "FooBar" `shouldBe` "fooBar"
       toCamel "helloWorld123" `shouldBe` "helloWorld123"

   it "VarName for a Predicate" $ do
       let (--->) x y = varNameForPred x `shouldBe` y
       IsIn "Num" (TyVar "a" 0) ---> "numa0"
       IsIn "Num" (TyCon "Int") ---> "numInt"
       IsIn "Monad" (TyCon "List") ---> "monadList"
       IsIn "Monad" (TyVar "m" 1) ---> "monadm1"

   it "Convert predicates for Num function" $ 
       "let f x = x + 1" --> [i|val f :: Num a -> a -> a
let f numT20 x = 
    (numT20 + x) (fromInteger numT20 1)
|]

   it "Convert predicates for curried function" $ 
       "let f x y = (x + 1, y + 2)" --> [i|val f :: Num a -> Num b -> a -> b -> (a, b)
let f numT50 numT60 x y = 
    ((numT50 + x) (fromInteger numT50 1), (numT60 + y) (fromInteger numT60 2))
|]

   it "Convert predicates for recursive function" $ 
       "let fac n = if n == 0 then 1 else n * (fac (n - 1))" --> 
           [i|val fac :: Eq a -> Num a -> a -> a
let fac eqT140 numT140 n = 
    if (eqT140 == n) (fromInteger numT140 0)
        then fromInteger numT140 1
        else (numT140 * n) (fac eqT140 numT140 ((numT140 - n) (fromInteger numT140 1)))
|]

   it "Convert predicates for recursive function 2" $ 
       "let fib n = if n == 0 then 1 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)" --> 
           [i|val fib :: Eq a -> Num a -> Num b -> a -> b
let fib eqT280 numT280 numT30 n = 
    if (eqT280 == n) (fromInteger numT280 0)
        then fromInteger numT30 1 else 
            if (eqT280 == n) (fromInteger numT280 1)
                then fromInteger numT30 1
                else numT30 + fib eqT280 numT280 numT30 ((numT280 - n) (fromInteger numT280 1)) (fib eqT280 numT280 numT30 ((numT280 - n) (fromInteger numT280 2)))
|]

   it "Convert predicate for let value" $
       "let x = 42" --> [i|val x :: Int
let x = fromInteger numInt 42
|]

   it "Instance construction - Equality of Tuples" $
       "let main = (2, 3) == (4, 7)" --> [i|val main :: Bool
let main = 
    eqTuple2 eqInt eqInt == (fromInteger numInt 2, fromInteger numInt 3) ((fromInteger numInt 4, fromInteger numInt 7))
|]

-- TODO: Work in progress 
--    it "Instance construction - Function Equality of Tuples" $
--        "let f x y = (x, y) == (x, y)" --> [i|val f :: Eq a -> Eq b -> a -> b -> Bool
-- let f eqT80 eqT90 x y = 
--     eqTuple2 eqT80 eqT90 == (x, y) (x, y)
-- |]

   it "Regression Test" $
    [i|
let foldl f v xs =
  if (null xs) then v
  else foldl f (f v (head xs)) (tail xs)

let reverse xs = foldl (\\xs x -> x : xs) [] xs

let partition n xs =
    let partition' n acc xs =
      if (n == 0 || null xs) then (reverse acc, xs)
          else partition' (n - 1) ((head xs) : acc) (tail xs) in
    partition' n [] xs|] --> [i|val foldl :: (a -> b -> a) -> a -> List b -> a
let foldl f v xs = if null xs then v
    else foldl f (f v (head xs)) (tail xs)

val reverse :: List a -> List a
let reverse xs = ((foldl (\\xs x ->
                          x : xs)) []) xs

val partition :: Eq a -> Num a -> a -> List b -> (List b, List b)
let partition eqT440 numT440 n xs = 
    let partition' n acc xs = 
                if (eqT440 == n) (fromInteger numT440 0) || null xs
                    then (reverse acc, xs)
                    else ((partition' ((numT440 - n) (fromInteger numT440 1))) (head xs : acc)) (tail xs) in
    partition' n [] xs
|]