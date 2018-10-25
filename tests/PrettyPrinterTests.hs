
module PrettyPrinterTests where

import Test.Hspec
import PrettyPrinter
import Parser (parseExpr)

(-->) :: String -> String -> Expectation
(-->) x y = either id pretty (parseExpr x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Pretty Printer Tests" $ do

    it "Print a lit" $ do
      "42" --> "42"
      "True" --> "True"
  
    it "Print a var" $ do
       "x" --> "x"
       "foo" --> "foo"

    it "Print a lam" $ do
       "\\x -> x" --> "\\x -> x"
       "\\x -> \\y -> y" --> "\\x -> \\y -> y"

    it "Print an app" $ do
       "x y" --> "x y"
       "x y z" --> "x y z"
       "x (y z)" --> "x (y z)"
       "x (y (z k))" --> "x (y (z k))"

    it "Print an app with lam" $ do
       "(\\x -> x) 42" --> "(\\x -> x) 42"
       "(\\x -> x) (\\y -> y)" --> "(\\x -> x) (\\y -> y)"
       "(\\x -> x) (f y)" --> "(\\x -> x) (f y)"

    it "Print a let" $ do
       "let n = 4 in n" --> "let n = 4 in n"
       "let f = \\x -> x + 1 in f" --> "let f = \\x -> + x 1 in f"
       "(let x = 4 in x) (let y = 5 in y)" --> "(let x = 4 in x) (let y = 5 in y)"

    it "Print an if then else" $ do
      "if true then 5 else 6" --> "if true then 5 else 6"
