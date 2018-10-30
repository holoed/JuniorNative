 module ClosureConversionTests where

import Test.Hspec
import Fixpoint
import ClosedAst
import ClosureConversion (convert)
import PrettyPrinter
import Parser (parseExpr)

close :: String -> Either String (Fix ClosedExpF)
close s = parseExpr s >>= convert

(-->) :: String -> String -> Expectation
(-->) x y = either id prettyClosed (close x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Closure Conversion Tests" $ do

    it "close a literal" $
      "42" --> "42"

    it "close a lambda with no free vars" $
      "\\x -> x" --> "mkEnv '_env0' [] \\x -> x"

    it "close a lambda with a free vars" $
      "\\x -> \\y -> x" --> "mkEnv '_env0' [] \\x -> mkEnv '_env1' [x] \\y -> lookupEnv '_env1' 0"

    it "close a lambda with more than a free vars" $
      "\\x -> \\y -> \\z -> (x, y, z)" -->
         "mkEnv '_env0' [] \\x -> mkEnv '_env1' [x] \\y -> mkEnv '_env2' [lookupEnv '_env1' 0, y] \\z -> (lookupEnv '_env1' 0, lookupEnv '_env2' 1, z)"

    it "close a let binding" $ do
      "let x = 42 in x" --> "let x = 42 in x"
      "let f = \\x -> let g = \\y -> x + y in g 5 in f" -->
        "let f = mkEnv '_env0' [+] \\x -> let g = mkEnv '_env1' [lookupEnv '_env0' 0, x] \\y -> lookupEnv '_env0' 0 lookupEnv '_env1' 1 y in g 5 in f"
      "let f = \\x -> let y = x + 1 in y + x in f" -->
         "let f = mkEnv '_env0' [+] \\x -> let y = lookupEnv '_env0' 0 x 1 in lookupEnv '_env0' 0 y x in f"

 
