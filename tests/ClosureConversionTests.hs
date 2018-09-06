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

    it "close a let binding" $ do
      "let x = 42 in x" --> "(Inl (Let \"x\" (Inl (Lit (I 42))) (Inl (Var \"x\"))))"
      "let f = \\x -> let g = \\y -> x + y in g 5 in f" --> "(Inl (Let \"f\" (Inr (MakeClosure (Inr (MakeEnv \"_env0\" [(Inl (Var \"+\"))])) (Inl (Lam \"x\" (Inl (Let \"g\" (Inr (MakeClosure (Inr (MakeEnv \"_env1\" [(Inr (LookupEnv \"_env0\" 0)),(Inl (Var \"x\"))])) (Inl (Lam \"y\" (Inl (App (Inl (App (Inr (LookupEnv \"_env0\" 0)) (Inr (LookupEnv \"_env1\" 1)))) (Inl (Var \"y\")))))))) (Inl (App (Inl (Var \"g\")) (Inl (Lit (I 5))))))))))) (Inl (Var \"f\"))))"
      "let f = \\x -> let y = x + 1 in y + x in f" --> "(Inl (Let \"f\" (Inr (MakeClosure (Inr (MakeEnv \"_env0\" [(Inl (Var \"+\"))])) (Inl (Lam \"x\" (Inl (Let \"y\" (Inl (App (Inl (App (Inr (LookupEnv \"_env0\" 0)) (Inl (Var \"x\")))) (Inl (Lit (I 1))))) (Inl (App (Inl (App (Inr (LookupEnv \"_env0\" 0)) (Inl (Var \"y\")))) (Inl (Var \"x\")))))))))) (Inl (Var \"f\"))))"

 
