{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module InterpreterTests where

import Data.String.Interpolate ( i )
import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation)
import Interpreter (interpretModule)
import InterpreterMonad (Result(..), InterpreterEnv, Prim(..), showResult, member, lookup)
import Parser (parseExpr)
import SynExpToExp (toExp)
import Location ( PString )
import Data.HashMap.Strict (fromList)
import Data.Maybe ( maybeToList )
import PAst ( SynExp, SynExpF(VarPat, Defn) )
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import Data.Text (pack, unpack)
import Prelude hiding (lookup)


env :: InterpreterEnv
env = (fromList [
    ("==", Function(\(Value x) -> return $ Function (\(Value y) -> return $ Value (B (x == y))))),
    ("/=", Function(\(Value x) -> return $ Function (\(Value y) -> return $ Value (B (x /= y))))),
    ("*", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x * y))))),
    ("-", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x - y))))),
    ("+", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x + y))))),
    (":", Function(\x -> return $ Function (\(List xs) -> return $ List (x:xs) ))),
    ("head", Function(\(List xs) -> return $ head xs)),
    ("tail", Function(\(List xs) -> return $ List (tail xs))),
    ("null", Function(\(List xs) -> return $ Value (B $ null xs))),
    ("[]", List [])], fromList [])

extractName :: SynExp -> Maybe String
extractName (In (Ann _ (Defn _ (In (Ann _ (VarPat n)):_) _))) = Just n
extractName _ = Nothing

run :: String -> Either PString [Result]
run code = do ast <- parseExpr code
              env' <- interpretModule env (toExp <$> ast)
              return $ getItem ast env'
    where getItem ast xs = maybeToList $
           if member "it" xs then lookup "it" xs
           else extractName (last ast) >>= ((`lookup` xs) . pack)

(-->) :: String -> String -> Expectation
(-->) code v  = either show (unpack . showResult . List) (run code) `shouldBe` v

tests :: SpecWith ()
tests =
  describe "Interpreter tests" $ do

    it "Literal" $ do
        "42" --> "[42]"

    it "let Value" $ do
        "let x = 42" --> "[42]"
        "let x = \"Hello\"" --> "[\"Hello\"]"

    it "let Function" $ do
        "let f x = x" --> "[<function>]"

    it "Applied function" $ do
        [i|let fac n = if n == 0 then 1 else n * (fac (n - 1))
           let main = fac 5 |] --> "[120]"

    it "Two dependent bindings" $ do
        [i|let x = 42
           let y = x + 1|] --> "[43]"

    it "Recursive higher order function" $ do
        [i|let foldl f v xs =
                 if (null xs) then v
                else foldl f (f v (head xs)) (tail xs)
  
           let main = foldl (*) 1 (1:2:3:4:5:[])  |] --> "[120]"

    it "Lambda tuple pattern" $ do
        [i|
            let f (x, y) = x + y
            let main = f (2, 3)
        |] --> "[5]"

    it "Lambda many tuples pattern" $ do
        [i|
            let f (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
            let main = f (2, 3) (5, 6)
        |] --> "[(7,9)]"
