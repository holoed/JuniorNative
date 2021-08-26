{-# LANGUAGE QuasiQuotes #-}
module ClosureConversionTests where

import Data.String.Interpolate ( i )
import Test.Hspec ( describe, it, shouldBe, SpecWith )
import ClosureConversion ( convertProg )
import Compiler (frontEnd)
import Intrinsics (classEnv, env)
import InterpreterMonad (empty)
import CompilerMonad (run)
import TypedAst (TypedExp)
import SynExpToExp ( fromExp )
import PrettyPrinter (prettyPrint)
import Annotations (mapAnn)
import Data.Map (keysSet)
import Data.Char (isSpace)

typeOf :: String -> IO [TypedExp]
typeOf code = do
    (ret, _, _) <- run (frontEnd code) (empty, classEnv) (env, [])
    return $ either (error . show) id ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

process :: String -> IO [String]
process code = (toString <$>) . convertProg (keysSet env) <$> typeOf code

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

tests :: SpecWith ()
tests =
  describe "Closure Conversion Tests" $ do

    it "convert lit bool" $ do
      xs <- process "let x = True"
      xs `shouldBe` ["let x = True"]

    it "convert lit num" $ do
      xs <- process "let x = 42"
      xs `shouldBe` ["let x = fromInteger 42"]

    it "convert simple function with no free vars" $ do
      xs <- process "let f x = x"
      xs `shouldBe` (map trim . lines)
       [i|let _f0 (_env, x) = x
          let f = let _c0 = MkClosure _f0 in _c0|]

    it "convert simple function with one free var" $ do
      xs <- process "let f x y = x + y"
      unlines xs `shouldBe` drop 1 [i|
let _f1 (_env, y) = 
    ClosureRef ((+) (GetEnv (\"x\", _env))) y
let _f0 (_env, x) = let _c0 = MkClosure _f1 in
                    SetEnv (\"x\", x, _c0)
let f = let _c1 = MkClosure _f0 in _c1
|]

    it "convert simple function with two free var" $ do
      xs <- process "let f x y z = x + y + z"
      unlines xs `shouldBe` drop 1 [i|
let _f2 (_env, z) = 
    ClosureRef ((+) (ClosureRef ((+) (GetEnv (\"x\", _env))) (GetEnv (\"y\", _env)))) z
let _f1 (_env, y) = let _c0 = MkClosure _f2 in
                    SetEnv ((\"x\", GetEnv (\"x\", _env), SetEnv (\"y\", y, _c0)))
let _f0 (_env, x) = let _c1 = MkClosure _f1 in
                    SetEnv (\"x\", x, _c1)
let f = let _c2 = MkClosure _f0 in _c2
|]

    it "convert simple recursive function" $ do
      xs <- process [i|let fac n = if n == 0 then 1 else n * fac (n - 1)
                       let main = fac 5 |]
      unlines xs `shouldBe` drop 1 [i|
let _f0 (_env, n) = 
    if ClosureRef (== n) (fromInteger 0)
        then fromInteger 1
        else ClosureRef ((*) n) (fac (ClosureRef (- n) (fromInteger 1)))
let fac = let _c0 = MkClosure _f0 in _c0
let main = fac (fromInteger 5)
|]