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

typeOf :: String -> IO [TypedExp]
typeOf code = do
    (ret, _, _) <- run (frontEnd code) (empty, classEnv) (env, [])
    return $ either (error . show) id ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

tests :: SpecWith ()
tests =
  describe "Closure Conversion Tests" $ do

    it "convert simple function" $ do
      xs <- typeOf "let f x = x"
      ((toString <$>) . convertProg) xs `shouldBe` ["let _c0 = MkClosure \"_f0\"","let _f0 (_env, x) = x"]