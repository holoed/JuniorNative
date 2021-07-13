{-# LANGUAGE QuasiQuotes #-}
module ModulePrinterTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( pipeline )
import CompilerMonad ( run )
import Intrinsics ( env, classEnv )
import Data.String.Interpolate ( i )

build :: String -> IO String
build code = do
   (Right x, _, _) <- run (pipeline code) classEnv env
   return (fst x)

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests =
  describe "Pretty Types Tests" $ do

    it "Pretty one binding" $ do
        "let f x = x" --> [i|val f :: a -> a
let f = \\x -> x
|]

    it "Pretty many binding" $ do
        [i|
let foldl f v xs =
  if (null xs) then v
  else foldl f (f v (head xs)) (tail xs)

let foldr f v xs =
  if (null xs) then v
  else f (head xs) (foldr f v (tail xs))|] --> [i|val foldl :: (a -> b -> a) -> a -> List b -> a
let foldl = \\f -> \\v -> \\xs -> if null xs then v else foldl f (f v (head xs)) (tail xs)

val foldr :: (a -> b -> b) -> b -> List a -> b
let foldr = \\f -> \\v -> \\xs -> if null xs then v else f (head xs) (foldr f v (tail xs))
|]