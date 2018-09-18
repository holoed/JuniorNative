module AlphaRenameTests where

import Test.Hspec
import AlphaRename
import Parser (parseExpr)

rn :: String -> String
rn s = either id show (fmap rename (parseExpr s))

(-->) :: String -> String -> Expectation
(-->) x y = (rn x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Alpha Rename Tests" $ do

     it "Rename identity" $
       "\\x -> x" --> "(Lam \"x0\" (Var \"x0\"))"

     it "Rename simple lambda shadowing" $
       "\\x -> \\x -> x" --> "(Lam \"x0\" (Lam \"x1\" (Var \"x1\")))"
