module OptimizeTypeClassesTests where

import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( fullJSClosedANF )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text (unpack)

build :: String -> IO String
build code = do
   (x, _, _) <- run (fullJSClosedANF code) (Interp.env, classEnv) (env, [])
   either (return . show) (return . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Optimize resolved Type Classes Known Instances" $ do

   it "value" $ "let main = 2 == 3" --> "42"