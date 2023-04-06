{-# LANGUAGE QuasiQuotes #-}
module UnitTests.ClosureConversionSpec where

import Data.String.Interpolate ( i )
import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Junior.Compiler.Intrinsics (classEnv, env)
import Junior.Interpreter.InterpreterMonad (empty)
import Junior.Compiler.CompilerMonad (run, CompileM)
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Parser.SynExpToExp ( fromExp )
import Junior.Pretty.Printer (prettyPrint)
import Junior.Utils.Annotations (mapAnn)
import Data.Char (isSpace)
import Junior.Compiler.CompilerSteps (desugarPredicates, closureConversion)
import Junior.Compiler.Compiler (step, frontEnd)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

closed :: String -> CompileM [TypedExp]
closed = frontEnd >=>
       step "desugar constraints" desugarPredicates >=>
       step "closure conversion" closureConversion 

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) ("", empty) (classEnv, env, [], [])
    return $ either (error . show) (toString <$>) ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

tests :: TopSpec
tests = parallel $
  describe "Closure Conversion Tests" $ do

    it "convert lit bool" $ do
      xs <- liftIO $ process "let x = True"
      xs `shouldBe` ["let x = True"]

    it "convert lit num" $ do
      xs <- liftIO $ process "let x = 42"
      xs `shouldBe` ["let x = \n    AppClosure ((AppClosure (fromInteger, numInt), 42))"]

    it "convert simple function with no free vars" $ do
      xs <- liftIO $ process "let f x = x"
      xs `shouldBe` (map trim . lines)
       [i|let _f0 (_env, x0) = x0 
          let f = MkClosure _f0|]

    it "convert simple function with one free var" $ do
      xs <- liftIO $ process "let f x y = (x, y)"
      unlines xs `shouldBe` "let _f1 (_env, y1) = (GetEnv (\"x0\", _env), y1)\nlet _f0 (_env, x0) = let _c0 = MkClosure _f1 in\n                     SetEnv (\"x0\", x0, _c0)\nlet f = MkClosure _f0\n"

    it "convert simple function with two free var" $ do
      xs <- liftIO $ process "let f x y z = (x, y, z)"
      unlines xs `shouldBe` "let _f2 (_env, z2) = \n    (GetEnv (\"x0\", _env), GetEnv (\"y1\", _env), z2)\nlet _f1 (_env, y1) = let _c0 = MkClosure _f2 in\n                     SetEnv ((\"x0\", GetEnv (\"x0\", _env), SetEnv (\"y1\", y1, _c0)))\nlet _f0 (_env, x0) = let _c1 = MkClosure _f1 in\n                     SetEnv (\"x0\", x0, _c1)\nlet f = MkClosure _f0\n"

    it "convert simple recursive function" $ do
      xs <- liftIO $ process [i|let fac n = if n == 0 then 1 else n * fac (n - 1)
                       let main = fac 5 |]
      unlines xs `shouldBe` "let _f2 (_env, n0) = \n    if AppClosure ((AppClosure ((AppClosure ((==, GetEnv (\"eqT14\", _env))), n0)), AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT14\", _env))), 0))))\n        then AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT14\", _env))), 1))\n        else AppClosure ((AppClosure ((AppClosure (((*), GetEnv (\"numT14\", _env))), n0)), AppClosure ((AppClosure ((AppClosure ((fac, GetEnv (\"eqT14\", _env))), GetEnv (\"numT14\", _env))), AppClosure ((AppClosure ((AppClosure ((-, GetEnv (\"numT14\", _env))), n0)), AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT14\", _env))), 1))))))))\nlet _f1 (_env, numT14) = \n    let _c0 = MkClosure _f2 in\n    SetEnv ((\"eqT14\", GetEnv (\"eqT14\", _env), SetEnv (\"numT14\", numT14, _c0)))\nlet _f0 (_env, eqT14) = let _c1 = MkClosure _f1 in\n                        SetEnv (\"eqT14\", eqT14, _c1)\nlet fac = MkClosure _f0\nlet main = \n    AppClosure ((AppClosure ((AppClosure (fac, eqInt), numInt)), AppClosure ((AppClosure (fromInteger, numInt), 5))))\n"

    it "applied function" $ do
      xs <- liftIO $ process [i|let f x = x + 1
                       let main = f 5 |]
      unlines xs `shouldBe` "let _f1 (_env, x0) = \n    AppClosure ((AppClosure ((AppClosure (((+), GetEnv (\"numT2\", _env))), x0)), AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT2\", _env))), 1))))\nlet _f0 (_env, numT2) = let _c0 = MkClosure _f1 in\n                        SetEnv (\"numT2\", numT2, _c0)\nlet f = MkClosure _f0\nlet main = \n    AppClosure ((AppClosure (f, numInt), AppClosure ((AppClosure (fromInteger, numInt), 5))))\n"

    it "tuple pattern in let" $ do
      xs <- liftIO $ process [i|
let add (z1, z2) = 
  let (a, b) = z1 in
  let (c, d) = z2 in
  (a + c, b + d)
  
let main = add ((2, 3), (4, 5)) |]
      unlines xs `shouldBe` "let _f2 (_env, (z10, z21)) = let (a2, b3) = z10 in\n                             let (c4, d5) = z21 in\n                             (AppClosure ((AppClosure ((AppClosure (((+), GetEnv (\"numT8\", _env))), a2)), c4)), AppClosure ((AppClosure ((AppClosure (((+), GetEnv (\"numT9\", _env))), b3)), d5)))\nlet _f1 (_env, numT9) = let _c0 = MkClosure _f2 in\n                        SetEnv ((\"numT8\", GetEnv (\"numT8\", _env), SetEnv (\"numT9\", numT9, _c0)))\nlet _f0 (_env, numT8) = let _c1 = MkClosure _f1 in\n                        SetEnv (\"numT8\", numT8, _c1)\nlet add = MkClosure _f0\nlet main = \n    AppClosure ((AppClosure ((AppClosure (add, numInt), numInt)), ((AppClosure ((AppClosure (fromInteger, numInt), 2)), AppClosure ((AppClosure (fromInteger, numInt), 3))), (AppClosure ((AppClosure (fromInteger, numInt), 4)), AppClosure ((AppClosure (fromInteger, numInt), 5))))))\n"

    it "convert nested function" $ do
      xs <- liftIO $ process "let f x = let g y = x + y in g 5"
      unlines xs `shouldBe` "let _f2 (_env, y2) = \n    AppClosure ((AppClosure ((AppClosure (((+), GetEnv (\"numT2\", _env))), GetEnv (\"x0\", _env))), y2))\nlet _f1 (_env, x0) = \n    let g1 = let _c0 = MkClosure _f2 in\n             SetEnv ((\"numT2\", GetEnv (\"numT2\", _env), SetEnv (\"x0\", x0, _c0))) in\n    AppClosure ((g1, AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT2\", _env))), 5))))\nlet _f0 (_env, numT2) = let _c1 = MkClosure _f1 in\n                        SetEnv (\"numT2\", numT2, _c1)\nlet f = MkClosure _f0\n"

    it "convert higher-order function" $ do
      xs <- liftIO $ process "let apply f x = f x\nlet addOne x = x + 1\nlet main = apply addOne 5"
      unlines xs `shouldBe` "let _f1 (_env, x0) = \n    AppClosure ((AppClosure ((AppClosure (((+), GetEnv (\"numT2\", _env))), x0)), AppClosure ((AppClosure ((fromInteger, GetEnv (\"numT2\", _env))), 1))))\nlet _f0 (_env, numT2) = let _c0 = MkClosure _f1 in\n                        SetEnv (\"numT2\", numT2, _c0)\nlet _f3 (_env, x2) = \n    AppClosure ((GetEnv (\"f1\", _env), x2))\nlet _f2 (_env, f1) = let _c2 = MkClosure _f3 in\n                     SetEnv (\"f1\", f1, _c2)\nlet addOne = MkClosure _f0\nlet apply = MkClosure _f2\nlet main = \n    AppClosure ((AppClosure ((apply, AppClosure (addOne, numInt))), AppClosure ((AppClosure (fromInteger, numInt), 5))))\n"
