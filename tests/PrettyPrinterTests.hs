module PrettyPrinterTests where

import Test.Hspec
import PrettyPrinter
import Parser (parseExpr)

(-->) :: String -> String -> Expectation
(-->) x y = either id pretty (parseExpr x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Pretty Printer Tests" $ do

    it "Print a var" $
       "x" --> "x"

    it "Print a lam" $
       "\\x -> x" --> "(\\x -> x)"

    it "Print an app" $ do
       "x y" --> "(x y)"
       "x y z" --> "((x y) z)"
