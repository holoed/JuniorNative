module AlphaRenameTests where

import Test.Hspec
import AlphaRename
import PrettyPrinter
import Parser (parseExpr)
import SynExpToExp (fromExp, toExp)

rn :: String -> String
rn s = either id pretty (fmap (fromExp . rename . toExp . head) (parseExpr s))

(-->) :: String -> String -> Expectation
(-->) x y = (rn x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Alpha Rename Tests" $ do

     it "Rename identity" $
       "\\x -> x" --> "\\x0 -> x0"

     it "Rename simple lambda shadowing" $
       "\\x -> \\x -> x" --> "\\x0 -> \\x1 -> x1"

     it "Rename curry function" $
       "\\f -> \\x -> \\y -> f (x, y)" --> "\\f0 -> \\x1 -> \\y2 -> f0 (x1, y2)"

     it "Rename a tuple" $
       "\\x -> \\y -> (x, y)" --> "\\x0 -> \\y1 -> (x0, y1)"

     it "Rename a let" $ do
       "let x = 4" --> "let x0 = 4"
       "let f = \\x -> x in f 5" --> "let f0 = \\x1 -> x1 in f0 5"
