module InterpreterTests where

import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation)
import Primitives ( Prim(I, B) ) 
import Interpreter (interpret, Result(..), Env)
import Parser (parseExpr)
import SynExpToExp (toExp)
import Location ( PString )
import Data.Map (fromList)

env :: Env
env = fromList [
    ("==", Function(\(Value x) -> return $ Function (\(Value y) -> return $ Value (B (x == y))))),
    ("*", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x * y))))),
    ("-", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x - y)))))
 ]

run :: String -> Either PString [Result]
run code = do ast <- parseExpr code
              sequence (interpret env <$> (toExp <$> ast))

instance Show Result where
  show (Value x) = show x
  show (Function _) = "<function>"


(-->) :: String -> String -> Expectation
(-->) code v  = either show show (run code) `shouldBe` v

tests :: SpecWith ()
tests =
  describe "Interpreter tests" $ do

    it "Literal" $ do 
        "42" --> "[I 42]"

    it "let Value" $ do
        "let x = 42" --> "[I 42]"

    it "let Function" $ do
        "let f x = x" --> "[<function>]"

    it "Applied function" $ do
        "let fac n = if n == 0 then 1 else n * (fac (n - 1)) in fac 5" --> "[I 120]"