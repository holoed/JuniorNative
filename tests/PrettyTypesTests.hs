module PrettyTypesTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import PrettyTypes (prettyQ)
import Types (Type(..), Qual(..), Pred(..), tyLam)
import TypesPrinter ()
import Data.Set (fromList)

(-->) :: Qual Type -> String -> Expectation
(-->) qt s = show qt `shouldBe` s

tests :: SpecWith ()
tests =
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