module ClosureConversionTests where

import Test.Hspec
import Fixpoint
import ClosedAst
import ClosureConversion (convert)
import Parser (parseExpr)

close :: String -> Either String (Fix ClosedExpF)
close s = parseExpr s >>= convert

(-->) :: String -> String -> Expectation
(-->) x y = either id show (close x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Closure Conversion Tests" $ do

    it "close a literal" $
      "42" --> "(Inl (Lit (I 42)))"

    it "close a lambda with no free vars" $
      "\\x -> x" --> "(Inr (MakeClosure (Inr (MakeEnv [])) (Inr (ClosedLam \"_env0\" \"x\" (Inl (Var \"x\"))))))"

    it "close a lambda with a free vars" $ do
      "\\x -> \\y -> x" --> "(Inr (MakeClosure (Inr (MakeEnv [])) (Inr (ClosedLam \"_env0\" \"x\" (Inr (MakeClosure (Inr (MakeEnv [(Inl (Var \"x\"))])) (Inr (ClosedLam \"_env1\" \"y\" (Inr (LookupEnv (Inl (Var \"_env1\")) 0))))))))))"
      "\\x -> \\y -> \\z -> (x, y, z)" --> "(Inr (MakeClosure (Inr (MakeEnv [])) (Inr (ClosedLam \"_env0\" \"x\" (Inr (MakeClosure (Inr (MakeEnv [(Inl (Var \"x\"))])) (Inr (ClosedLam \"_env1\" \"y\" (Inr (MakeClosure (Inr (MakeEnv [(Inl (Var \"x\")),(Inl (Var \"y\"))])) (Inr (ClosedLam \"_env2\" \"z\" (Inl (MkTuple [(Inr (LookupEnv (Inl (Var \"_env1\")) 0)),(Inr (LookupEnv (Inl (Var \"_env2\")) 1)),(Inl (Var \"z\"))]))))))))))))))"
