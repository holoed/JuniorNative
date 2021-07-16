module SymbolTableTests where

import SymbolTable (Symbol)
import CompilerMonad (run)
import Compiler (pipeline)
import Intrinsics ( env, classEnv )
import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation )

compile :: String -> IO [Symbol]
compile code = do
   (_, (_, ss), _) <- run (pipeline code) classEnv (env, [])
   return ss

(-->) :: String -> [String] -> Expectation 
(-->) x y = do ret <- compile x
               show <$> ret `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Symbol Table Tests" $ do

      it "value binding" $  "let x = 42" --> [
       "Symbol {name = x Just line 1 column 5, ty = Num a => a, parent = Nothing, top = True}",
       "Symbol {name = fromInteger Just line 1 column 9, ty = Num a => Int -> a, parent = Nothing, top = False}",
       "Symbol {name = 42 Just line 1 column 9, ty = Int, parent = Nothing, top = False}"  
       ]

      it "function binding" $  "let f x = x" --> [
         "Symbol {name = f Just line 1 column 5, ty = a -> a, parent = Nothing, top = True}",
         "Symbol {name = x Just line 1 column 7, ty = a, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 11, ty = a, parent = Just (Symbol {name = x Just line 1 column 7, ty = a, parent = Nothing, top = False}), top = False}"
         ]

      it "function application" $  "let f x y = x + y" --> [
         "Symbol {name = f Just line 1 column 5, ty = Num a => a -> a -> a, parent = Nothing, top = True}",
         "Symbol {name = x Just line 1 column 7, ty = Num a => a, parent = Nothing, top = False}",
         "Symbol {name = y Just line 1 column 9, ty = Num a => a, parent = Nothing, top = False}",
         "Symbol {name = + Just line 1 column 15, ty = Num a => a -> a -> a, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 13, ty = Num a => a, parent = Just (Symbol {name = x Just line 1 column 7, ty = Num a => a, parent = Nothing, top = False}), top = False}",
         "Symbol {name = y Just line 1 column 17, ty = Num a => a, parent = Just (Symbol {name = y Just line 1 column 9, ty = Num a => a, parent = Nothing, top = False}), top = False}"
         ]