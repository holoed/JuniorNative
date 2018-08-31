module FreeVariablesTests where

import Test.Hspec
import Fixpoint
import Annotations
import Ast
import FreeVariables
import Data.Set

tests :: SpecWith ()
tests =
  describe "Free Variables Tests" $ do

    it "Free vars of a literal" $
      freeVars (lit $ I 42) `shouldBe` In (Ann (fromList []) (Lit $ I 42))

    it "Free vars of a variable" $
      freeVars (var "x") `shouldBe` In (Ann (fromList ["x"]) (Var "x"))

    it "Free vars of a tuple" $
      freeVars (mkTuple [var "x", var "y"]) `shouldBe`
        In (Ann (fromList ["x", "y"]) $ MkTuple [In (Ann (fromList ["x"]) (Var "x")), In (Ann (fromList ["y"]) (Var "y"))])

    it "Free vars of an application" $
      freeVars (app (var "f") (var "x")) `shouldBe`
        In (Ann (fromList ["f", "x"]) $ App (In (Ann (fromList ["f"]) (Var "f"))) (In (Ann (fromList ["x"]) (Var "x"))))

    it "Free vars of a lambda" $ do
      freeVars (lam "x" (var "x")) `shouldBe` In (Ann (fromList []) (Lam "x" (In (Ann (fromList ["x"]) (Var "x")))))
      freeVars (lam "x" (lam "y" (var "x"))) `shouldBe`
        In (Ann (fromList []) (Lam "x" (In (Ann (fromList ["x"]) (Lam "y" (In (Ann (fromList ["x"]) (Var "x"))))))))

    it "Free vars of a let" $ do
      freeVars (leT "x" (lit $ I 42) (var "x")) `shouldBe`
        In (Ann (fromList []) (Let "x" (In (Ann (fromList []) (Lit $ I 42))) (In (Ann (fromList ["x"]) (Var "x")))))
      freeVars (leT "x" (lit $ I 42) (leT "y" (lit $ I 24) (var "x"))) `shouldBe`
        In (Ann (fromList []) (Let "x" (In (Ann (fromList []) (Lit $ I 42)))
          (In (Ann (fromList ["x"]) (Let "y" (In (Ann (fromList []) (Lit $ I 24))) (In (Ann (fromList ["x"]) (Var "x"))))))))

    it "Free vars of if then else" $
      freeVars (ifThenElse (var "x") (var "y") (var "z")) `shouldBe`
        In (Ann (fromList ["x", "y", "z"]) (IfThenElse (In (Ann (fromList ["x"]) (Var "x")))
                                                       (In (Ann (fromList ["y"]) (Var "y")))
                                                       (In (Ann (fromList ["z"]) (Var "z")))))
