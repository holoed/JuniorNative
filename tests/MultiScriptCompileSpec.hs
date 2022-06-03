{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MultiScriptCompileSpec where

import Data.String.Interpolate ( i )
import Test.Hspec (Spec, shouldBe, describe, it, Expectation, parallel)
import Compiler ( fullJSClosedANF )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified InterpreterIntrinsics as Interp (env)
import Data.Text ( pack, unpack, Text )
import JavaScriptRunner (runJS)
import Location (PString)
import Environment (Env)
import Control.Monad ( foldM )

build :: Either PString (Text, Env) -> Text -> IO (Either PString (Text, Env))
build (Right (js, env1)) code = do
   (x, (env2, _), _) <- run (fullJSClosedANF (unpack code)) (Interp.env, classEnv) (env1, [])
   return $ (\js2 -> (js <> pack "\r\n" <> js2, env2)) <$> x
build (Left x) _ = return $ Left x

exec :: Text -> IO String
exec = do
   let libPath = "src/javascript/baseClosedLib.js"
   runJS libPath . unpack

buildAll :: [Text] -> IO (Either PString (Text, Env))
buildAll = foldM build (Right (pack "", env))

(-->) :: [Text] -> String -> Expectation
(-->) s1 s2 = do (Right (js, _)) <- buildAll s1
                 ret <- exec js
                 ret `shouldBe` s2

spec :: Spec
spec = parallel $ do
  describe "Multi script compilation tests" $ do

   it "single script" $ ["let main = 42"] --> "42"

   it "two scripts" $ ["let x = 12", 
                       "let main = x + 1"] --> "13"

   -- TODO: Fix name clash created by closure conversion and ANF transformation
   it "two scripts with functions" $ 
                     ["let fac n = if n == 0 then 1 else n * fac (n - 1)",
                      pack [i|let fib n = 
                                if n == 0 then 1
                                else if n == 1 then 1
                                else fib (n - 1) + fib (n - 2)|], 
                      "let main = (fac 5, fib 6)"] --> "13"