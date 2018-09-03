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
      "\\x -> x" --> "(Inr (MakeClosure (Inr (MakeEnv \"_env0\" [])) (Inl (Lam \"x\" (Inl (Var \"x\"))))))"

    it "close a lambda with a free vars" $
      "\\x -> \\y -> x" --> "(Inr (MakeClosure (Inr (MakeEnv \"_env0\" [])) (Inl (Lam \"x\" (Inr (MakeClosure (Inr (MakeEnv \"_env1\" [(Inl (Var \"x\"))])) (Inl (Lam \"y\" (Inr (LookupEnv \"_env1\" 0))))))))))"

    it "close a lambda with more than a free vars" $
      "\\x -> \\y -> \\z -> (x, y, z)" -->
           ("(Inr (MakeClosure (Inr (MakeEnv \"_env0\" [])) " ++
           "(Inl (Lam \"x\" (Inr (MakeClosure (Inr (MakeEnv \"_env1\" [(Inl (Var \"x\"))])) " ++
           "(Inl (Lam \"y\" (Inr (MakeClosure (Inr (MakeEnv \"_env2\" [(Inr (LookupEnv \"_env1\" 0)),(Inl (Var \"y\"))])) " ++
           "(Inl (Lam \"z\" (Inl (MkTuple [(Inr (LookupEnv \"_env1\" 0)),(Inr (LookupEnv \"_env2\" 1)),(Inl (Var \"z\"))]))))))))))))))")

    it "close a let binding" $
      "let x = 42 in x" --> "(Inl (Let \"x\" (Inl (Lit (I 42))) (Inl (Var \"x\"))))"
