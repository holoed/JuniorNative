{-# LANGUAGE QuasiQuotes #-}
module CompilerTests where

import Data.String.Interpolate ( i )
import Test.Hspec (SpecWith, shouldBe, describe, it, Expectation)
import Compiler ( full )
import CompilerMonad ( run )
import Intrinsics (env, classEnv)
import qualified Data.Map as Map (fromList) 
import Interpreter (InterpreterEnv, Result(..))
import Primitives (Prim(..))

ienv :: InterpreterEnv
ienv = Map.fromList [
    ("numInt", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x + y))))),
    ("fromInteger", Function(\_ -> return $ Function (\(Value x) -> return $ Value x))),
    ("+", Function(\(Function f) -> 
           return $ Function(\x -> 
           return $ Function (\y -> 
             do (Function f') <- f x
                f' y ))))
 ]

build :: String -> IO String
build code = do
   (x, _, _) <- run (full code) (ienv, classEnv) (env, [])
   return $ either show id x

(-->) :: String -> String -> Expectation
(-->) s1 s2 = build s1 >>= (`shouldBe` s2)

tests :: SpecWith ()
tests = do
  describe "Compiler Tests" $ do

   it "value" $ "let x = 42" --> "[I 42]"

   it "function" $ "let f x = x + 1" --> "[<function>]"

   it "applied function" $ [i|
      let f x = x + 1
      let ret = f 5
   |] --> "[I 6]"
