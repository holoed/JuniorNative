{-# LANGUAGE QuasiQuotes #-}
module ModulePrinterSpec where

import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import Compiler ( frontEndPrinted )
import CompilerMonad ( run )
import Intrinsics ( env, classEnv )
import Data.String.Interpolate ( i )
import InterpreterMonad (empty) 
import Data.Text (unpack)

build :: String -> IO String
build code = do
   (Right x, _, _) <- run (frontEndPrinted code) ("main", empty) (classEnv, env, [], [])
   return (unpack x)

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

spec :: Spec
spec = parallel $
  describe "Pretty Types Tests" $ do

    it "Pretty one binding" $ do
        "let f x = x" --> [i|val f :: a -> a
let f x = x
|]

    it "Pretty many binding" $ do
        [i|
let foldr f v xs =
  if (null xs) then v
  else f (head xs) (foldr f v (tail xs))|] --> [i|val foldr :: (a -> b -> b) -> b -> List a -> b
let foldr f v xs = if null xs then v
    else f (head xs) (foldr f v (tail xs))
|]