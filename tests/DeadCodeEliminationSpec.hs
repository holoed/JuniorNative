{-# LANGUAGE QuasiQuotes #-}
module DeadCodeEliminationSpec where

import CompilerMonad ( run, CompileM )
import Data.Text (Text, unpack)
import Data.String.Interpolate (i)
import Compiler (frontEnd, step)
import Intrinsics (env, classEnv)
import Control.Monad ((>=>))
import CompilerSteps (prettyPrintModule, deadCodeElimin)
import qualified InterpreterIntrinsics as Interp (env)
import Test.Hspec (Spec, shouldBe, describe, it, Expectation)

compile :: String -> CompileM Text
compile = frontEnd >=>
            step "dead code elimination" deadCodeElimin >=>
            step "pretty print module" prettyPrintModule

build :: String -> IO String
build code = do
   (x, _, _) <- run (compile code) (Interp.env, classEnv) (env, [])
   either (return . show) (return . unpack) x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

spec :: Spec
spec = do
  describe "Dead Code Elimination Tests" $ do

   it "eliminate unused let" $ "let main = let x = 42 in 3" --> [i|val main :: Int
let main = fromInteger 3
|]

   it "eliminate unused other let" $ "let main = let x = 42 in let y = 12 in x" --> [i|val main :: Int
let main = let x = fromInteger 42 in x
|]

   it "nothing to eliminate" $ "let main = let x = 42 in let y = 12 in (x, y)" --> [i|val main :: (Int, Int)
let main = let x = fromInteger 42 in
           let y = fromInteger 12 in
           (x, y)
|]