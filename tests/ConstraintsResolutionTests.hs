{-# LANGUAGE QuasiQuotes #-}
module ConstraintsResolutionTests where

import Annotations (mapAnn)
import Types ( Pred(IsIn), Type(TyApp, TyVar, TyCon) ) 
import ConstraintsResolution (typeForPred, toCamel, varNameForPred, getNewArgs)
import Test.Hspec ( it, describe, shouldBe, SpecWith, Expectation )
import TypesPrinter () 
import Intrinsics ( classEnv, env )
import Data.String.Interpolate ( i )
import Compiler ( backendPrinted )
import CompilerMonad ( run )
import InterpreterMonad (empty)
import Data.Text (unpack)
import BuiltIns (tupleCon)
import SynExpToExp ( fromExp )
import PrettyPrinter ( prettyPrint )

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
       IsIn "Num" (TyVar "a" 0) ---> "numa"
       IsIn "Num" (TyCon "Int") ---> "numInt"
       IsIn "Monad" (TyCon "List") ---> "monadList"
       IsIn "Monad" (TyVar "m" 1) ---> "monadm"

   it "getNewArgs" $ do
       let testGetNewArgs xs = prettyPrint . fromExp . mapAnn fst <$> getNewArgs classEnv xs
       testGetNewArgs [IsIn "Eq" (TyCon "Int")] `shouldBe` ["eqInt"]   
       testGetNewArgs [IsIn "Eq" (tupleCon [TyCon "Int", TyCon "Int"])] `shouldBe` ["eqTuple2 eqInt eqInt"] 
       testGetNewArgs [IsIn "Eq" (tupleCon [tupleCon [TyCon "Int", TyCon "Int"], TyCon "Int"])] `shouldBe` ["eqTuple2 (eqTuple2 eqInt eqInt) eqInt"] 

   it "Convert predicates for Num function" $ 
       "let f x = x + 1" --> [i|val f :: Num a -> a -> a
let f numT2 x = 
    (numT2 + x) (fromInteger numT2 1)
|]

   it "Convert predicates for curried function" $ 
       "let f x y = (x + 1, y + 2)" --> [i|val f :: Num a -> Num b -> a -> b -> (a, b)
let f numT5 numT6 x y = 
    ((numT5 + x) (fromInteger numT5 1), (numT6 + y) (fromInteger numT6 2))
|]

   it "Convert predicates for recursive function" $ 
       "let fac n = if n == 0 then 1 else n * (fac (n - 1))" --> 
           [i|val fac :: Eq a -> Num a -> a -> a
let fac eqT14 numT14 n = 
    if (eqT14 == n) (fromInteger numT14 0)
        then fromInteger numT14 1
        else (numT14 * n) (fac eqT14 numT14 ((numT14 - n) (fromInteger numT14 1)))
|]

   it "Convert predicates for recursive function 2" $ 
       "let fib n = if n == 0 then 1 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)" --> 
           [i|val fib :: Eq a -> Num a -> Num b -> a -> b
let fib eqT28 numT28 numT3 n = 
    if (eqT28 == n) (fromInteger numT28 0)
        then fromInteger numT3 1 else 
            if (eqT28 == n) (fromInteger numT28 1)
                then fromInteger numT3 1
                else numT3 + fib eqT28 numT28 numT3 ((numT28 - n) (fromInteger numT28 1)) (fib eqT28 numT28 numT3 ((numT28 - n) (fromInteger numT28 2)))
|]

   it "Convert predicate for let value" $
       "let x = 42" --> [i|val x :: Int
let x = fromInteger numInt 42
|]

-- TODO: Working on this
--    it "Instance construction - Nested tuples" $
--        "let main = ((1,2),3) == ((1,2), 3)" --> [i|val main :: Bool
-- let main = 
--     eqTuple2 (eqTuple2 eqInt eqInt) eqInt == ((fromInteger numInt 1, fromInteger numInt 2), fromInteger numInt 3) (((fromInteger numInt 1, fromInteger numInt 2), fromInteger numInt 3))
-- |]

   it "Instance construction - Function Equality of Tuples" $
       "let f x y = (x, y) == (x, y)" --> [i|val f :: Eq a -> Eq b -> a -> b -> Bool
let f eqT8 eqT9 x y = 
    eqTuple2 eqT8 eqT9 == (x, y) (x, y)
|]

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
let partition eqT44 numT44 n xs = 
    let partition' n acc xs = 
                if (eqT44 == n) (fromInteger numT44 0) || null xs
                    then (reverse acc, xs)
                    else ((partition' ((numT44 - n) (fromInteger numT44 1))) (head xs : acc)) (tail xs) in
    partition' n [] xs
|]