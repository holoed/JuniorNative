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

      it "Simple let" $  "let x = 42" --> [
          "Symbol {name = x Just line 1 column 1, ty = Num a => a, parent = Nothing, top = True}",
          "Symbol {name = x Just line 1 column 5, ty = a, parent = Nothing, top = False}",
          "Symbol {name = fromInteger Just line 1 column 9, ty = Num a => Int -> a, parent = Nothing, top = False}",
          "Symbol {name = I 42 Just line 1 column 9, ty = Int, parent = Nothing, top = False}",
          "Symbol {name = x Just line 1 column 5, ty = a, parent = Just (Symbol {name = x Just line 1 column 1, ty = Num a => a, parent = Nothing, top = True}), top = False}"
       ]