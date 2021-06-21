module FreeVariablesTests where

import Test.Hspec ( describe, it, shouldBe, SpecWith )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Primitives ( Prim(I) )
import Ast ( lit, var, mkTuple, app, lam, leT, ifThenElse, ExpF(Lit, Var, MkTuple, App, Lam, Let, IfThenElse), Loc(..), ExpLoc(..) )
import FreeVariables ( freeVars )
import Data.Set ( empty, fromList )

zeroLoc :: Loc
zeroLoc = Loc 0 0 0

tests :: SpecWith ()
tests =
  describe "Free Variables Tests" $ do 
    
    it "Free vars of a literal" $
      freeVars empty (lit zeroLoc $ I 42) `shouldBe` In (Ann (LitLoc zeroLoc, fromList []) (Lit $ I 42))

    it "Free vars of a variable" $
      freeVars empty (var zeroLoc "x") `shouldBe` In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x"))

    it "Free vars of a tuple" $
      freeVars empty (mkTuple zeroLoc [var zeroLoc "x", var zeroLoc "y"]) `shouldBe`
        In (Ann (TupleLoc zeroLoc, fromList ["x", "y"]) $ MkTuple [In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x")), In (Ann (VarLoc zeroLoc, fromList ["y"]) (Var "y"))])

    it "Free vars of an application" $
      freeVars empty (app (var zeroLoc "f") (var zeroLoc "x")) `shouldBe`
        In (Ann (AppLoc, fromList ["f", "x"]) $ App (In (Ann (VarLoc zeroLoc, fromList ["f"]) (Var "f"))) (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x"))))

    it "Free vars of a lambda" $ do
      freeVars empty (lam zeroLoc ("x", zeroLoc) (var zeroLoc "x")) `shouldBe` In (Ann (LamLoc zeroLoc zeroLoc ,fromList []) (Lam "x" (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x")))))
      freeVars empty (lam zeroLoc ("x", zeroLoc) (lam zeroLoc ("y", zeroLoc) (var zeroLoc "x"))) `shouldBe`
        In (Ann (LamLoc zeroLoc zeroLoc, fromList []) (Lam "x" (In (Ann (LamLoc zeroLoc zeroLoc, fromList ["x"]) (Lam "y" (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x"))))))))

    it "Free vars of a let" $ do
      freeVars empty (leT zeroLoc ("x", zeroLoc) (lit zeroLoc $ I 42) (var zeroLoc "x")) `shouldBe`
        In (Ann (LetLoc zeroLoc zeroLoc, fromList []) (Let "x" (In (Ann (LitLoc zeroLoc, fromList []) (Lit $ I 42))) (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x")))))
      freeVars empty (leT zeroLoc ("x", zeroLoc) (lit zeroLoc $ I 42) (leT zeroLoc ("y", zeroLoc) (lit zeroLoc $ I 24) (var zeroLoc "x"))) `shouldBe`
        In (Ann (LetLoc zeroLoc zeroLoc, fromList []) (Let "x" (In (Ann (LitLoc zeroLoc, fromList []) (Lit $ I 42)))
          (In (Ann (LetLoc zeroLoc zeroLoc, fromList ["x"]) (Let "y" (In (Ann (LitLoc zeroLoc, fromList []) (Lit $ I 24))) (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x"))))))))

    it "Free vars of if then else" $
      freeVars empty (ifThenElse zeroLoc (var zeroLoc "x") (var zeroLoc "y") (var zeroLoc "z")) `shouldBe`
        In (Ann (IfThenElseLoc zeroLoc, fromList ["x", "y", "z"]) (
                               IfThenElse (In (Ann (VarLoc zeroLoc, fromList ["x"]) (Var "x")))
                                          (In (Ann (VarLoc zeroLoc, fromList ["y"]) (Var "y")))
                                          (In (Ann (VarLoc zeroLoc, fromList ["z"]) (Var "z")))))
