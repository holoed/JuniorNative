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
      xs `shouldBe` ["let x = \n    let anf_0 = AppClosure (fromInteger, numInt) in\n    let anf_1 = 42 in AppClosure (anf_0, anf_1)"]

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
    let anf_0 = AppClosure ((==, GetEnv (\"eqT14\", _env))) in
    let anf_3 = AppClosure (anf_0, n) in
    let anf_1 = AppClosure ((fromInteger, GetEnv (\"numT14\", _env))) in
    let anf_2 = 0 in
    let anf_4 = AppClosure (anf_1, anf_2) in
    let anf_18 = AppClosure (anf_3, anf_4) in 
        if anf_18
            then let anf_5 = AppClosure ((fromInteger, GetEnv (\"numT14\", _env))) in
                 let anf_6 = 1 in
                 AppClosure (anf_5, anf_6)
            else let anf_7 = AppClosure (((*), GetEnv (\"numT14\", _env))) in
                 let anf_16 = AppClosure (anf_7, n) in
                 let anf_8 = AppClosure ((fac, GetEnv (\"eqT14\", _env))) in
                 let anf_14 = AppClosure ((anf_8, GetEnv (\"numT14\", _env))) in
                 let anf_9 = AppClosure ((-, GetEnv (\"numT14\", _env))) in
                 let anf_12 = AppClosure (anf_9, n) in
                 let anf_10 = AppClosure ((fromInteger, GetEnv (\"numT14\", _env))) in
                 let anf_11 = 1 in
                 let anf_13 = AppClosure (anf_10, anf_11) in
                 let anf_15 = AppClosure (anf_12, anf_13) in
                 let anf_17 = AppClosure (anf_14, anf_15) in
                 AppClosure (anf_16, anf_17)
let _f1 (_env, numT14) = 
    let _c0 = MkClosure _f2 in
    SetEnv ((\"eqT14\", GetEnv (\"eqT14\", _env), SetEnv (\"numT14\", numT14, _c0)))
let _f0 (_env, eqT14) = let _c1 = MkClosure _f1 in
                        SetEnv (\"eqT14\", eqT14, _c1)
let fac = let _c2 = MkClosure _f0 in _c2
let main = let anf_19 = AppClosure (fac, eqInt) in
           let anf_22 = AppClosure (anf_19, numInt) in
           let anf_20 = AppClosure (fromInteger, numInt) in
           let anf_21 = 5 in
           let anf_23 = AppClosure (anf_20, anf_21) in
           AppClosure (anf_22, anf_23)
|]

    it "applied function" $ do
      xs <- process [i|let f x = x + 1
                       let main = f 5 |]
      unlines xs `shouldBe` drop 1 [i|
let _f1 (_env, x) = 
    let anf_0 = AppClosure (((+), GetEnv (\"numT2\", _env))) in
    let anf_3 = AppClosure (anf_0, x) in
    let anf_1 = AppClosure ((fromInteger, GetEnv (\"numT2\", _env))) in
    let anf_2 = 1 in
    let anf_4 = AppClosure (anf_1, anf_2) in
    AppClosure (anf_3, anf_4)
let _f0 (_env, numT2) = let _c0 = MkClosure _f1 in
                        SetEnv (\"numT2\", numT2, _c0)
let f = let _c1 = MkClosure _f0 in _c1
let main = let anf_7 = AppClosure (f, numInt) in
           let anf_5 = AppClosure (fromInteger, numInt) in
           let anf_6 = 5 in
           let anf_8 = AppClosure (anf_5, anf_6) in
           AppClosure (anf_7, anf_8)
|]