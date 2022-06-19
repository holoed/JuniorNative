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

build :: Either PString (Text, Env) -> (String, Text) -> IO (Either PString (Text, Env))
build (Right (js, env1)) (ns, code) = do
   (x, (env2, _, _), _) <- run (fullJSClosedANF (unpack code)) (ns, Interp.env, classEnv) (env1, [], [])
   return $ (\js2 -> (js <> pack "\r\n" <> js2, env2)) <$> x
build (Left x) _ = return $ Left x

exec :: Text -> IO String
exec = do
   let libPath = "src/javascript/baseClosedLib.js"
   runJS libPath . unpack

buildAll :: [(String, Text)] -> IO (Either PString (Text, Env))
buildAll = foldM build (Right (pack "", env))

(-->) :: [(String, Text)] -> String -> Expectation
(-->) s1 s2 = do (Right (js, _)) <- buildAll s1
                 ret <- exec js
                 ret `shouldBe` s2

spec :: Spec
spec = parallel $ do
  describe "Multi script compilation tests" $ do

   it "single script" $ [("main", "let main = 42")] --> "42"

   it "two scripts" $ [("lib", "let x = 12"), 
                       ("main", "let main = x + 1")] --> "13"
                       
   it "two scripts with functions" $ 
                     [("lib1", "let fac n = if n == 0 then 1 else n * fac (n - 1)"),
                      ("lib2", pack [i|let fib n = 
                                if n == 0 then 1
                                else if n == 1 then 1
                                else fib (n - 1) + fib (n - 2)|]), 
                      ("lib3", "let main = (fac 5, fib 10)")] --> "[120,89]"