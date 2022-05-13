module ParserTests where

import Parser (parseExpr)
import Test.Hspec (SpecWith, shouldBe, describe, it)
import Annotations (Ann(Ann))
import Location (Loc (Loc), PString (PStr))
import Fixpoint (Fix(In))
import PAst (SynExpF(Lit, VarPat, Defn, InfixApp, Var))
import Primitives (Prim(I))
import Types (Type(TyCon, TyApp, TyVar), Qual((:=>)), Pred (IsIn), tyLam)
import Data.Set (fromList)
import qualified Operators (Fixity (Infix), Associativity(Right))

tests :: SpecWith ()
tests = do
  describe "Parser Tests" $ do

   it "Literals" $
     parseExpr "42" `shouldBe`  Right [In (Ann (Just (Loc 2 1 1)) (Lit (I 42)))]

   it "Let binding" $
     parseExpr "let x = 42" `shouldBe` 
       Right [In (Ann (Just (Loc 3 1 1)) 
         (Defn Nothing [In (Ann (Just (Loc 1 1 5)) (VarPat "x"))] 
           (In (Ann (Just (Loc 2 1 9)) (Lit (I 42))))))]

   it "Let binding with type signature" $
     parseExpr "val x :: Int\r\nlet x = 42" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [] :=> TyCon "Int")) [In (Ann (Just (Loc 1 2 5)) (VarPat "x"))] 
           (In (Ann (Just (Loc 2 2 9)) (Lit (I 42))))))]

   it "Let binding with type signature 2" $
     parseExpr "val x :: List Int\r\nlet xs = 42:[]" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [] :=> TyApp (TyCon "List") (TyCon "Int"))) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",12, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 3" $
     parseExpr "val x :: Num a => List a\r\nlet xs = 42:[]" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [IsIn "Num" (TyVar "a" 0)] :=> TyApp (TyCon "List") (TyVar "a" 0))) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",12, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 4" $
     parseExpr "val x :: (Eq a, Num a) => List a\r\nlet xs = 42:[]" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Num" (TyVar "a" 0)] :=> TyApp (TyCon "List") (TyVar "a" 0))) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",12, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 5" $
     parseExpr "val f :: a -> a\r\nlet f x = x" `shouldBe` 
       Right [(In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0))) [(In (Ann (Just (Loc 1 2 5)) (VarPat "f"))),(In (Ann (Just (Loc 1 2 7)) (VarPat "x")))] (In (Ann (Just (Loc 1 2 11)) (Var "x"))))))]
  
  it "Let binding with invalid type signature" $
     parseExpr "val f :: \\x -> x\r\nlet f x = x" `shouldBe` Left(PStr ("Invalid type signature at ", Just (Loc 1 1 10)))