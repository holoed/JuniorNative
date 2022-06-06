module PrettyTypesSpec where

import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import PrettyTypes (prettyQ)
import Types (Type(..), Qual(..), Pred(..), tyLam)
import TypesPrinter ()
import Data.Set (fromList)

(-->) :: Qual Type -> String -> Expectation
(-->) qt s = show qt `shouldBe` s

spec :: Spec
spec = parallel $
  describe "Pretty Types Tests" $ do

    it "Pretty a bool" $ do
        prettyQ (fromList [] :=> TyCon "Bool") --> "Bool"

    it "Pretty a value" $ do
        prettyQ (fromList [] :=> TyVar "T1" 0) --> "a"

    it "Pretty a function" $ do
        prettyQ (fromList [] :=> tyLam (TyVar "T1" 0) (TyVar "T2" 0)) --> "a -> b"

    it "Pretty a type application" $ do
        prettyQ (fromList [] :=> TyApp (TyVar "T1" 1) (TyVar "T2" 0)) --> "a b"

    it "Pretty type parenthesized" $ do
        prettyQ (fromList [] :=> tyLam (tyLam (TyVar "T1" 0) (TyVar "T2" 0)) (TyVar "T3" 0)) --> "(a -> b) -> c"

    it "Pretty type with type class constraint" $ do
        prettyQ (fromList [IsIn "Num" (TyVar "T1" 0)] :=> TyVar "T1" 0) --> "Num a => a"

    it "Pretty type with multiple type class constraints" $ do
        prettyQ (fromList [IsIn "Num" (TyVar "T1" 0), IsIn "Ord" (TyVar "T1" 0)] :=> TyVar "T1" 0) --> "(Num a, Ord a) => a"

    it "Pretty type application" $ do
        prettyQ (fromList [] :=> TyApp (TyApp (TyCon "Map") (TyCon "String")) (TyCon "Int")) --> "Map String Int"

    it "Pretty type application 2" $ do
        prettyQ (fromList [] :=> TyApp (TyApp (TyCon "Map") (TyCon "String")) (TyApp (TyApp (TyCon "Map") (TyCon "String")) (TyCon "Int"))) --> "Map String (Map String Int)"

    it "Pretty type application combined with arrow" $ do
        prettyQ (fromList [] :=> tyLam (tyLam (TyCon "Int") (TyCon "Int")) (TyApp (TyCon "List") (TyCon "Int"))) --> "(Int -> Int) -> List Int"

    it "Pretty type application combined with arrow 2" $ do
        prettyQ (fromList [] :=> tyLam (tyLam (TyCon "Int") (TyApp (TyCon "List") (TyCon "Int"))) (TyApp (TyCon "List") (TyCon "Int"))) --> "(Int -> List Int) -> List Int"