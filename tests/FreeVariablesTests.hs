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
      freeVarsExp (lit $ I 42) `shouldBe` In (Ann (fromList []) (Lit $ I 42))

    it "Free vars of a variable" $
      freeVarsExp (var "x") `shouldBe` In (Ann (fromList ["x"]) (Var "x"))

    it "Free vars of a tuple" $
      freeVarsExp (mkTuple [var "x", var "y"]) `shouldBe`
        In (Ann (fromList ["x", "y"]) $ MkTuple [In (Ann (fromList ["x"]) (Var "x")), In (Ann (fromList ["x", "y"]) (Var "y"))])

    it "Free vars of an application" $
      freeVarsExp (app (var "f") (var "x")) `shouldBe`
        In (Ann (fromList ["f", "x"]) $ App (In (Ann (fromList ["f"]) (Var "f"))) (In (Ann (fromList ["f", "x"]) (Var "x"))))
