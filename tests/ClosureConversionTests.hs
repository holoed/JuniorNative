module ClosureConversionTests where

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

typeOf :: String -> IO [TypedExp]
typeOf code = do
    (ret, _, _) <- run (frontEnd code) (empty, classEnv) (env, [])
    return $ either (error . show) id ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

process :: String -> IO [String]
process code = (toString <$>) . convertProg (keysSet env) <$> typeOf code

tests :: SpecWith ()
tests =
  describe "Closure Conversion Tests" $ do

    it "convert lit bool" $ do
      xs <- process "let x = True"
      xs `shouldBe` ["let x = True"]

    it "convert lit num" $ do
      xs <- process "let x = 42"
      xs `shouldBe` ["let x = fromInteger 42"]

    it "convert simple function" $ do
      xs <- process "let f x = x"
      xs `shouldBe` ["let f = let _c0 = MkClosure \"_f0\"","let _f0 (_env, x) = x"]