{-# LANGUAGE QuasiQuotes #-}
module CompileToClosedJsTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( fullJSClosed )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)
import JavaScriptRunner (runJS)

build :: String -> IO String
build code = do
   let libPath = "src/javascript/baseClosedLib.js"
   (x, _, _) <- run (fullJSClosed code) (Interp.env, classEnv) (env, [])
   either (return . show) (runJS libPath . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Compile to Closed JavaScript Tests" $ do

   it "value" $ "let main = 42" --> "42"

   it "string value" $ "let main = \"Hello\"" --> "\"Hello\""

   it "char value" $ "let main = 'h'" --> "\"h\""

   it "applied function" $ [i|
      let f x = x + 1
      let main = f 5
   |] --> "6"