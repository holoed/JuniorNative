module UnitTests.JavaScriptRunnerSpec where

import System.Process ( readProcess ) 
import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel)
import JavaScriptRunner ( runJS )
import Control.Monad.IO.Class (liftIO)

tests :: TopSpec
tests = parallel $
  describe "Run JS test" $ do

    it "Run simple js and return a result" $ do
      ret <- liftIO $ readProcess "node" ["-"] "console.log(2 + 3);" 
      ret `shouldBe` "5\n"

    it "Run js using lib" $ do
      lib <- liftIO $ readFile "src/javascript/baselib.js"
      let jsCode = "const main = (((__add(numInt))(((fromInteger(numInt))(2))))(((fromInteger(numInt))(3))))"
      ret <- liftIO $ readProcess "node" ["-"] (lib ++ "\r\n\r\n" ++ jsCode ++ "\r\n\r\n" ++ "console.log(main);")
      ret `shouldBe` "5\n"

    it "Run js" $ do
      let jsCode = "const main = (((__add(numInt))(((fromInteger(numInt))(2))))(((fromInteger(numInt))(3))))"
      ret <- liftIO $ runJS "src/javascript/baselib.js" jsCode 
      ret `shouldBe` "5"