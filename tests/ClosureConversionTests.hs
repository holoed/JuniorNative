{-# LANGUAGE QuasiQuotes #-}
module ClosureConversionTests where

import Data.String.Interpolate ( i )
import Test.Hspec ( describe, it, shouldBe, SpecWith )
import Compiler (closed)
import Intrinsics (classEnv, env)
import InterpreterMonad (empty)
import CompilerMonad (run)
import TypedAst (TypedExp)
import SynExpToExp ( fromExp )
import PrettyPrinter (prettyPrint)
import Annotations (mapAnn)
import Data.Char (isSpace)

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) (empty, classEnv) (env, [])
    return $ either (error . show) (toString <$>) ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

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
      xs `shouldBe` ["let x = ClosureRef (fromInteger numInt) 42"]

    it "convert simple function with no free vars" $ do
      xs <- process "let f x = x"
      xs `shouldBe` (map trim . lines)
       [i|let _f0 (_env, x) = x
          let f = let _c0 = MkClosure _f0 in _c0|]

    it "convert simple function with one free var" $ do
      xs <- process "let f x y = (x, y)"
      unlines xs `shouldBe` drop 1 [i|
let _f1 (_env, y) = (GetEnv (\"x\", _env), y)
let _f0 (_env, x) = let _c0 = MkClosure _f1 in
                    SetEnv (\"x\", x, _c0)
let f = let _c1 = MkClosure _f0 in _c1
|]

    it "convert simple function with two free var" $ do
      xs <- process "let f x y z = (x, y, z)"
      unlines xs `shouldBe` drop 1 [i|
let _f2 (_env, z) = 
    (GetEnv (\"x\", _env), GetEnv (\"y\", _env), z)
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
let _f2 (_env, n) = 
    if ClosureRef (ClosureRef (== (GetEnv (\"eqT14\", _env))) n) (ClosureRef (fromInteger (GetEnv (\"numT14\", _env))) 0)
        then ClosureRef (fromInteger (GetEnv (\"numT14\", _env))) 1
        else ClosureRef (ClosureRef ((*) (GetEnv (\"numT14\", _env))) n) (ClosureRef (ClosureRef (fac (GetEnv (\"eqT14\", _env))) (GetEnv (\"numT14\", _env))) (ClosureRef (ClosureRef (- (GetEnv (\"numT14\", _env))) n) (ClosureRef (fromInteger (GetEnv (\"numT14\", _env))) 1)))
let _f1 (_env, numT14) = 
    let _c0 = MkClosure _f2 in
    SetEnv ((\"eqT14\", GetEnv (\"eqT14\", _env), SetEnv (\"numT14\", numT14, _c0)))
let _f0 (_env, eqT14) = let _c1 = MkClosure _f1 in
                        SetEnv (\"eqT14\", eqT14, _c1)
let fac = let _c2 = MkClosure _f0 in _c2
let main = 
    ClosureRef (ClosureRef (fac eqInt) numInt) (ClosureRef (fromInteger numInt) 5)
|]