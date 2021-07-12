module SymbolTableTests where

import SymbolTable (Symbol)
import CompilerMonad (run)
import Compiler (pipeline)
import Intrinsics ( env, classEnv )
import Data.Either ( fromRight )
import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation )

compile :: String -> IO [Symbol]
compile code = do
   (x, _, _) <- run (pipeline code) classEnv env
   return $ fromRight [] (snd <$> x)

(-->) :: String -> [String] -> Expectation 
(-->) x y = do ret <- compile x
               show <$> ret `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Symbol Table Tests" $ do

      it "value binding" $  "let x = 42" --> [
         "Symbol {name = x Just line 1 column 5, ty = Num a => a, parent = Nothing, top = True}",
         "Symbol {name = fromInteger Just line 1 column 9, ty = Num a => Int -> a, parent = Nothing, top = False}",
         "Symbol {name = 42 Just line 1 column 9, ty = Int, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 5, ty = Num a => a, parent = Just (Symbol {name = x Just line 1 column 5, ty = Num a => a, parent = Nothing, top = True}), top = True}"
       ]

      it "function binding" $  "let f x = x" --> [
         "Symbol {name = f Just line 1 column 5, ty = b -> b, parent = Nothing, top = True}",
         "Symbol {name = x Just line 1 column 7, ty = b, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 11, ty = b, parent = Just (Symbol {name = x Just line 1 column 7, ty = b, parent = Nothing, top = False}), top = False}",
         "Symbol {name = f Just line 1 column 5, ty = a -> a, parent = Just (Symbol {name = f Just line 1 column 5, ty = a -> a, parent = Nothing, top = True}), top = True}"]

      it "function application" $  "let f x y = x + y" --> [
         "Symbol {name = f Just line 1 column 5, ty = Num b => b -> b -> b, parent = Nothing, top = True}",
         "Symbol {name = x Just line 1 column 7, ty = Num b => b, parent = Nothing, top = False}",
         "Symbol {name = y Just line 1 column 9, ty = Num b => b, parent = Nothing, top = False}",
         "Symbol {name = + Just line 1 column 15, ty = Num b => b -> b -> b, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 13, ty = Num b => b, parent = Just (Symbol {name = x Just line 1 column 7, ty = Num b => b, parent = Nothing, top = False}), top = False}",
         "Symbol {name = y Just line 1 column 17, ty = Num b => b, parent = Just (Symbol {name = y Just line 1 column 9, ty = Num b => b, parent = Nothing, top = False}), top = False}",
         "Symbol {name = f Just line 1 column 5, ty = Num a => a -> a -> a, parent = Just (Symbol {name = f Just line 1 column 5, ty = Num a => a -> a -> a, parent = Nothing, top = True}), top = True}"
         ]