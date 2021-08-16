module JavaScriptRunnerTests where

import System.Process ( readProcess ) 
import Test.Hspec ( describe, it, shouldBe, SpecWith)
import JavaScriptRunner ( runJS )

tests :: SpecWith ()
tests =
  describe "Run JS test" $ do

    it "Run simple js and return a result" $ do
      ret <- readProcess "node" ["-"] "console.log(2 + 3);" 
      ret `shouldBe` "5\n"

    it "Run js using lib" $ do
      lib <- readFile "src/javascript/baselib.js"
      let jsCode = "const main = (((__add(numInt))(((fromInteger(numInt))(2))))(((fromInteger(numInt))(3))))"
      ret <- readProcess "node" ["-"] (lib ++ "\r\n\r\n" ++ jsCode ++ "\r\n\r\n" ++ "console.log(main);")
      ret `shouldBe` "5\n"

    it "Run js" $ do
      let jsCode = "const main = (((__add(numInt))(((fromInteger(numInt))(2))))(((fromInteger(numInt))(3))))"
      ret <- runJS "src/javascript/baselib.js" jsCode 
      ret `shouldBe` "5"