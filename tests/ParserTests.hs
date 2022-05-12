module ParserTests where

import Parser (parseExpr)
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Annotations (Ann(Ann))
import Data.Either (Either(Right))
import Location (Loc (Loc))
import Fixpoint (Fix(In))
import PAst (SynExpF(Lit, VarPat, Defn, InfixApp, Var))
import Primitives (Prim(I))
import Types (Type(TyCon), Qual((:=>)))
import Data.Set (fromList)
import Data.Data (Fixity(Infix))

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
         (Defn (Just List Int) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",12, Infix Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]