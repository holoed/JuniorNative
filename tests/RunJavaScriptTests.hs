module RunJavaScriptTests where

import System.Process ( readProcess ) 
import Test.Hspec ( describe, it, shouldBe, SpecWith)

tests :: SpecWith ()
tests =
  describe "Run JS test" $ do

    it "This a test" $ do
      ret <- readProcess "node" ["-"] "console.log(2 + 3);" 
      ret `shouldBe` "5\n"