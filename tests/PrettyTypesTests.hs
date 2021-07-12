module PrettyTypesTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import PrettyTypes (prettyQ)
import Types (Type(..), Qual(..), tyLam)
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
        