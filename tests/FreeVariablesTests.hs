module FreeVariablesTests where

import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation)
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann), mapAnn )
import Primitives ( Prim(I) )
import Ast ( lit, var, varPat, mkTuple, app, lam, leT, ifThenElse, ExpF(Lit, Var, VarPat, MkTuple, App, Lam, Let, IfThenElse), Loc(..) )
import FreeVariables ( freeVars )
import Data.Set (Set(), empty, fromList )
import Parser (parseExpr)
import SynExpToExp (toExp)

(-->) :: String -> Fix (Ann (Set String) ExpF) -> Expectation
(-->) s v  = (mapAnn snd <$> either error (freeVars empty . toExp <$>) (parseExpr s)) `shouldBe` [v]

zeroLoc :: Loc
zeroLoc = Loc 0 0 0

tests :: SpecWith ()
tests =
  describe "Free Variables Tests" $ do

    it "Free vars of a literal" $
      "42" --> In (Ann (fromList []) (Lit $ I 42))

    it "Free vars of a variable" $
      "x" --> In (Ann (fromList ["x"]) (Var "x"))

    it "Free vars of a tuple" $
      "(x, y)" --> In (Ann (fromList ["x", "y"]) $ 
        MkTuple [In (Ann (fromList ["x"]) (Var "x")), 
                 In (Ann (fromList ["y"]) (Var "y"))])

    it "Free vars of an application" $
      "f x" -->
        In (Ann (fromList ["f", "x"]) $ 
          App (In (Ann (fromList ["f"]) (Var "f"))) 
              (In (Ann (fromList ["x"]) (Var "x"))))

    it "Free vars of a lambda" $ do
      freeVars empty (lam zeroLoc (varPat zeroLoc "x") (var zeroLoc "x")) `shouldBe` In (Ann (Just zeroLoc ,fromList []) (Lam (In (Ann (Just zeroLoc, fromList ["x"]) (VarPat "x"))) (In (Ann (Just zeroLoc, fromList ["x"]) (Var "x")))))
      freeVars empty (lam zeroLoc (varPat zeroLoc "x") (lam zeroLoc (varPat zeroLoc "y") (var zeroLoc "x"))) `shouldBe`
        In (Ann (Just zeroLoc, fromList []) (Lam (In (Ann (Just zeroLoc, fromList ["x"]) (VarPat "x"))) (In (Ann (Just zeroLoc, fromList ["x"]) (Lam (In (Ann (Just zeroLoc, fromList ["y"]) (VarPat "y"))) (In (Ann (Just zeroLoc, fromList ["x"]) (Var "x"))))))))

    it "Free vars of a let" $ do
      freeVars empty (leT zeroLoc (varPat zeroLoc "x") (lit zeroLoc $ I 42) (var zeroLoc "x")) `shouldBe`
        In (Ann (Just zeroLoc, fromList []) (Let (In (Ann (Just zeroLoc, fromList ["x"]) (VarPat "x"))) (In (Ann (Just zeroLoc, fromList []) (Lit $ I 42))) (In (Ann (Just zeroLoc, fromList ["x"]) (Var "x")))))
      freeVars empty (leT zeroLoc (varPat zeroLoc "x") (lit zeroLoc $ I 42) (leT zeroLoc (varPat zeroLoc "y") (lit zeroLoc $ I 24) (var zeroLoc "x"))) `shouldBe`
        In (Ann (Just zeroLoc, fromList []) (Let (In (Ann (Just zeroLoc, fromList ["x"]) (VarPat "x"))) (In (Ann (Just zeroLoc, fromList []) (Lit $ I 42)))
          (In (Ann (Just zeroLoc, fromList ["x"]) (Let (In (Ann (Just zeroLoc, fromList ["y"]) (VarPat "y"))) (In (Ann (Just zeroLoc, fromList []) (Lit $ I 24))) (In (Ann (Just zeroLoc, fromList ["x"]) (Var "x"))))))))

    it "Free vars of if then else" $
      freeVars empty (ifThenElse zeroLoc (var zeroLoc "x") (var zeroLoc "y") (var zeroLoc "z")) `shouldBe`
        In (Ann (Just zeroLoc, fromList ["x", "y", "z"]) (
                               IfThenElse (In (Ann (Just zeroLoc, fromList ["x"]) (Var "x")))
                                          (In (Ann (Just zeroLoc, fromList ["y"]) (Var "y")))
                                          (In (Ann (Just zeroLoc, fromList ["z"]) (Var "z")))))
