{-# LANGUAGE QuasiQuotes #-}
module ParserSpec where

import Data.String.Interpolate (i)
import Parser (parseExpr)
import Test.Hspec (Spec, shouldBe, describe, it, parallel)
import Annotations (Ann(Ann))
import Location (Loc (Loc), PString (PStr))
import Fixpoint (Fix(In))
import PAst (SynExpF(Lit, VarPat, Defn, InfixApp, Var, TypeDecl, Match, MatchExp, TuplePat))
import Primitives (Prim(I))
import Types (Type(TyCon, TyApp, TyVar), Qual((:=>)), Pred (IsIn), tyLam)
import Data.Set (fromList)
import qualified Operators (Fixity (Infix), Associativity(Right))

spec :: Spec
spec = parallel $ do
  describe "Parser Tests" $ do

   it "Literals" $
     parseExpr "42" `shouldBe`  Right [In (Ann (Just (Loc 2 1 1)) (Lit (I 42)))]

   it "List syntax 0" $
     parseExpr "[]" `shouldBe`  Right [In (Ann (Just (Loc 2 1 1)) (Var "[]"))]

   it "List syntax 1" $
     parseExpr "1:[]" `shouldBe` Right [(In (Ann (Just (Loc 1 1 2)) (InfixApp (":",5, Operators.Infix Operators.Right) (In (Ann (Just (Loc 1 1 1)) (Lit (I 1)))) (In (Ann (Just (Loc 2 1 3)) (Var "[]"))))))]

   it "List syntax 2" $
     parseExpr "[1]" `shouldBe`  Right [(In (Ann (Just (Loc 1 1 1)) (InfixApp (":",12, Operators.Infix Operators.Right) (In (Ann (Just (Loc 1 1 2)) (Lit (I 1)))) (In (Ann (Just (Loc 1 1 1)) (Var "[]"))))))]

   it "List syntax 3" $
     parseExpr "[1, 2]" `shouldBe` Right [(In (Ann (Just (Loc 1 1 1)) (InfixApp (":",12, Operators.Infix Operators.Right) (In (Ann (Just (Loc 1 1 2)) (Lit (I 1)))) (In (Ann (Just (Loc 1 1 1)) (InfixApp (":",12, Operators.Infix Operators.Right) (In (Ann (Just (Loc 1 1 5)) (Lit (I 2)))) (In (Ann (Just (Loc 1 1 1)) (Var "[]")))))))))]

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
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",5, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 3" $
     parseExpr "val x :: Num a => List a\r\nlet xs = 42:[]" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [IsIn "Num" (TyVar "a" 0)] :=> TyApp (TyCon "List") (TyVar "a" 0))) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",5, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 4" $
     parseExpr "val x :: (Eq a, Num a) => List a\r\nlet xs = 42:[]" `shouldBe` 
       Right [In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Num" (TyVar "a" 0)] :=> TyApp (TyCon "List") (TyVar "a" 0))) [In (Ann (Just (Loc 2 2 5)) (VarPat "xs"))] 
           (In (Ann (Just (Loc 1 2 12)) (InfixApp (":",5, Operators.Infix Operators.Right) 
           (In (Ann (Just (Loc 2 2 10)) (Lit (I 42)))) 
           (In (Ann (Just (Loc 2 2 13)) (Var "[]"))))))))]

  it "Let binding with type signature 5" $
     parseExpr "val f :: a -> a\r\nlet f x = x" `shouldBe` 
       Right [(In (Ann (Just (Loc 3 2 1)) 
         (Defn (Just (fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0))) [(In (Ann (Just (Loc 1 2 5)) (VarPat "f"))),(In (Ann (Just (Loc 1 2 7)) (VarPat "x")))] (In (Ann (Just (Loc 1 2 11)) (Var "x"))))))]
  
  it "Let binding with invalid type signature" $
     parseExpr "val f :: \\x -> x\r\nlet f x = x" `shouldBe` Left(PStr ("Invalid type signature at ", Just (Loc 1 1 10)))

  it "data definition" $
     parseExpr "data Maybe a = Nothing | Just a" `shouldBe` Right [In (Ann (Just (Loc 4 1 1)) (TypeDecl (TyApp (TyCon "Maybe") (TyVar "a" 0)) [TyCon "Nothing", (TyApp (TyCon "Just") (TyVar "a" 0))] []))]

  it "data definition 2" $
     parseExpr "data Tree a = Leaf | Node (Tree a) a (Tree a)" `shouldBe` 
       Right [In (Ann (Just (Loc 4 1 1)) (TypeDecl (TyApp (TyCon "Tree") (TyVar "a" 0)) [
        TyCon "Leaf", 
        (TyApp (TyApp (TyApp (TyCon "Node") (TyApp (TyCon "Tree") (TyVar "a" 0))) (TyVar "a" 0)) (TyApp (TyCon "Tree") (TyVar "a" 0)))] []))]

  it "data definition with deriving" $
     parseExpr "data Maybe a = Nothing | Just a deriving Functor" `shouldBe` Right [In (Ann (Just (Loc 4 1 1)) (TypeDecl (TyApp (TyCon "Maybe") (TyVar "a" 0)) [TyCon "Nothing", (TyApp (TyCon "Just") (TyVar "a" 0))] ["Functor"]))]   

  it "pattern matching syntax 0" $
     parseExpr [i|
        let foo x = match x with y -> y
     |] `shouldBe` 
      Right  [In (Ann (Just (Loc 3 2 9)) (Defn Nothing [In (Ann (Just (Loc 3 2 13)) (VarPat "foo")),(In (Ann (Just (Loc 1 2 17)) (VarPat "x")))] (In (Ann (Just (Loc 5 2 21)) (Match (In (Ann (Just (Loc 1 2 27)) (Var "x"))) [(In (Ann (Just (Loc 2 2 36)) (MatchExp (In (Ann (Just (Loc 1 2 34)) (VarPat "y"))) (In (Ann (Just (Loc 1 2 39)) (Var "y"))))))])))))]


  it "pattern matching syntax 1" $
     parseExpr [i|
        let foo x = match x with 
                    | y -> y
     |] `shouldBe` 
      Right  [In (Ann (Just (Loc 3 2 9)) (Defn Nothing [In (Ann (Just (Loc 3 2 13)) (VarPat "foo")),(In (Ann (Just (Loc 1 2 17)) (VarPat "x")))] (In (Ann (Just (Loc 5 2 21)) (Match (In (Ann (Just (Loc 1 2 27)) (Var "x"))) [(In (Ann (Just (Loc 2 3 25)) (MatchExp (In (Ann (Just (Loc 1 3 23)) (VarPat "y"))) (In (Ann (Just (Loc 1 3 28)) (Var "y"))))))])))))]

  it "pattern matching syntax 2" $
     parseExpr [i|
        let foo x = match x with 
                    | y -> y
                    | z -> z
     |] `shouldBe` 
      Right  [In (Ann (Just (Loc 3 2 9)) (Defn Nothing [In (Ann (Just (Loc 3 2 13)) (VarPat "foo")),(In (Ann (Just (Loc 1 2 17)) (VarPat "x")))] 
        (In (Ann (Just (Loc 5 2 21)) (Match (In (Ann (Just (Loc 1 2 27)) (Var "x"))) [
        (In (Ann (Just (Loc 2 3 25)) (MatchExp (In (Ann (Just (Loc 1 3 23)) (VarPat "y"))) (In (Ann (Just (Loc 1 3 28)) (Var "y")))))),
        (In (Ann (Just (Loc 2 4 25)) (MatchExp (In (Ann (Just (Loc 1 4 23)) (VarPat "z"))) (In (Ann (Just (Loc 1 4 28)) (Var "z"))))))
        ])))))]

  it "pattern matching syntax 3" $
     parseExpr [i|
        let foo x = match x with 
                    | (y, z) -> z
                    
     |] `shouldBe` 
      Right   [(In (Ann (Just (Loc 3 2 9)) (Defn Nothing [In (Ann (Just (Loc 3 2 13)) (VarPat "foo")),(In (Ann (Just (Loc 1 2 17)) (VarPat "x")))] 
               (In (Ann (Just (Loc 5 2 21)) (Match (In (Ann (Just (Loc 1 2 27)) (Var "x"))) [
               (In (Ann (Just (Loc 2 3 30)) (MatchExp (In (Ann (Just (Loc 1 3 23)) (TuplePat [In (Ann (Just (Loc 1 3 24)) (VarPat "y")),(In (Ann (Just (Loc 1 3 27)) (VarPat "z")))]))) (In (Ann (Just (Loc 1 3 33)) (Var "z"))))))
                   ]))))))]