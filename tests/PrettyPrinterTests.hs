{-# LANGUAGE QuasiQuotes #-}
module PrettyPrinterTests where

import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation )
import SynExpToExp ( toExp, fromExp )
import PrettyPrinter ( prettyPrint )
import Parser (parseExpr)
import Data.List (intercalate)
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

(-->) :: [String] -> [String] -> Expectation
(-->) x y = either show (intercalate "\n" . map (trim . prettyPrint . fromExp . toExp))
                      (parseExpr (intercalate "\n" x))
            `shouldBe`
                      intercalate "\n" (map trim y)

tests :: SpecWith ()
tests =
  describe "Pretty Printer Tests" $ do

    it "Print a lit" $ do
      ["42"] --> ["42"]
      ["2.4"] --> ["2.4"]
      ["True"] --> ["True"]
      ["\"Hello\""] --> ["\"Hello\""]

    it "Print a var" $ do
      ["x"] --> ["x"]
      ["foo"] --> ["foo"]

    it "Print a lam" $ do
      ["\\x -> x"] --> ["\\x -> x"]
      ["\\x -> \\y -> y"] --> ["\\x y -> y"]
      ["\\x y -> y"] --> ["\\x y -> y"]

    it "Print an app" $ do
      ["x y"] --> ["x y"]
      ["x y z"] --> ["x y z"]
      ["x (y z)"] --> ["x (y z)"]
      ["x (y (z k))"] --> ["x (y (z k))"]

    it "Print an app with lam" $ do
      ["(\\x -> x) 42"] --> ["(\\x -> x) 42"]
      ["(\\x -> x) (\\y -> y)"] --> ["(\\x -> x) (\\y -> y)"]
      ["(\\x -> x) (f y)"] --> ["(\\x -> x) (f y)"]

    it "Print a let" $ do
      ["let n = 4"] --> ["let n = 4"]
      ["let f x = x + 1"] --> ["let f x = x + 1"]
      ["let f x y = (x, y)"] --> ["let f x y = (x, y)"]
      ["let x = 4",
       "let y = 5"] --> ["let x = 4",
                         "let y = 5"]

    it "Print an if then else" $ do
      ["if true then 5 else 6"] --> ["if true then 5 else 6"]
      ["(if true then f else g) 5"] --> ["(if true then f else g) 5"]
      ["f (if true then 5 else 6)"] --> ["f (if true then 5 else 6)"]
      ["if true then (if false then 5 else 6) else 7"] --> ["if true then if false then 5 else 6 else 7"]

    it "Print a mix" $
      ["let x = 4",
     "if true then 5 else 6"] --> ["let x = 4",
                                   "if true then 5 else 6"]

    it "Print a tuple" $ do
      ["(\\x -> x + 1, \\y -> y - 1)"] --> ["(\\x -> x + 1, \\y -> y - 1)"]
      ["\\x -> (x, \\y -> (y, x))"] --> ["\\x -> (x, \\y -> (y, x))"]

    it "Print operators" $ do
       ["2 > 3"] --> ["2 > 3"]
       ["2 < 4"] --> ["2 < 4"]
       ["2 == 3"] --> ["2 == 3"]

    it "Print Associativity" $ do
      ["2 + 3 > 1 + 2"] --> ["2 + 3 > 1 + 2"]
      ["xs ++ ys == ys ++ xs"] --> ["xs ++ ys == ys ++ xs"]
      ["3 > 2 && 4 < 5"] --> ["3 > 2 && 4 < 5"]
      ["3 > 2 || 4 < 5"] --> ["3 > 2 || 4 < 5"]
      ["3 > 2 || 4 < 5 && 2 > 3 || 4 > 5"] --> ["3 > 2 || 4 < 5 && 2 > 3 || 4 > 5"]
      ["(True || False) && (True || False)"] --> ["(True || False) && (True || False)"]
      ["True || False && True || False"] --> ["True || False && True || False"]
      ["True || (False && True) || False"] --> ["True || False && True || False"]

    it "Print symbol functions" $ do
      ["let (++) x y = (x, y)"] --> ["let (++) x y = (x, y)"]



