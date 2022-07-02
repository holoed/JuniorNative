module UnitTests.FreeVariablesSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel)
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann), mapAnn )
import Primitives ( Prim(I) )
import Ast ( ExpF(Lit, Var, VarPat, MkTuple, App, Lam, Let, IfThenElse, Defn, Match, MatchExp) )
import FreeVariables ( freeVars )
import Data.Set (Set(), empty, fromList )
import Parser (parseExpr)
import SynExpToExp (toExp)
import Data.Maybe (fromJust)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> Fix (Ann (Set String) ExpF) -> m ()
(-->) s v  = (mapAnn snd <$> either (error . show) (freeVars empty . (fromJust . toExp) <$>) (parseExpr s)) `shouldBe` [v]

tests :: TopSpec
tests = parallel $
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
      "\\x -> x" --> In (Ann (fromList []) (Lam (In (Ann (fromList ["x"]) (VarPat "x"))) (In (Ann (fromList ["x"]) (Var "x")))))
      "\\x -> \\y -> x" -->
        In (Ann (fromList []) (Lam (In (Ann (fromList ["x"]) (VarPat "x"))) (In (Ann (fromList ["x"]) 
                              (Lam (In (Ann (fromList ["y"]) (VarPat "y"))) (In (Ann (fromList ["x"]) (Var "x"))))))))

    it "Free vars of a let" $ do
      "let x = 42" -->
        In (Ann (fromList []) (Defn Nothing (In (Ann (fromList ["x"]) (VarPat "x"))) (In (Ann (fromList []) (Lit $ I 42)))))
      "let foo = let x = 42 in let y = 24 in x" --> 
        In (Ann (fromList []) (Defn Nothing (In (Ann (fromList ["foo"]) (VarPat "foo"))) (In (Ann (fromList []) (Let (In (Ann (fromList ["x"]) (VarPat "x"))) (In (Ann (fromList []) (Lit (I 42)))) (In (Ann (fromList ["x"]) (Let (In (Ann (fromList ["y"]) (VarPat "y"))) (In (Ann (fromList []) (Lit (I 24)))) (In (Ann (fromList ["x"]) (Var "x")))))))))))

    it "Free vars of if then else" $
      "if x then y else z" -->
        In (Ann (fromList ["x", "y", "z"]) (
                    IfThenElse (In (Ann (fromList ["x"]) (Var "x")))
                               (In (Ann (fromList ["y"]) (Var "y")))
                               (In (Ann (fromList ["z"]) (Var "z")))))

    it "Free vars of pattern matching" $
      "let f x = match x with y -> y" --> 
       In (Ann (fromList ["y"]) (Defn Nothing (In (Ann (fromList ["f"]) (VarPat "f"))) (In (Ann (fromList ["y"]) (Lam (In (Ann (fromList ["x"]) (VarPat "x"))) (In (Ann (fromList ["x","y"]) (Match (In (Ann (fromList ["x"]) (Var "x"))) [(In (Ann (fromList ["y"]) (MatchExp (In (Ann (fromList ["y"]) (VarPat "y"))) (In (Ann (fromList ["y"]) (Var "y"))))))]))))))))
