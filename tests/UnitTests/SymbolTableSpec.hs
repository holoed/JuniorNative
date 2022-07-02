module UnitTests.SymbolTableSpec where

import SymbolTable (Symbol)
import CompilerMonad (run)
import Compiler (frontEndPrinted)
import Intrinsics ( env, classEnv )
import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Control.Monad.Catch (MonadThrow)
import InterpreterMonad (empty) 
import Control.Monad.IO.Class (MonadIO (liftIO))

compile :: String -> IO [Symbol]
compile code = do
   (_, (_, _, ss, _), _) <- run (frontEndPrinted code) ("main", empty) (classEnv, env, [], [])
   return ss

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> [String] -> m() 
(-->) x y = do ret <- liftIO $ compile x
               (show <$> ret) `shouldBe` y

tests :: TopSpec
tests = parallel $
  describe "Symbol Table Tests" $ do

      it "value binding" $  "let x = 42" --> [
       "Symbol {name = x Just line 1 column 5, ty = Int, parent = Nothing, top = True}",
       "Symbol {name = fromInteger Just line 1 column 9, ty = Int -> Int, parent = Nothing, top = False}",
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
         "Symbol {name = x Just line 1 column 13, ty = a, parent = Just (Symbol {name = x Just line 1 column 7, ty = Num a => a, parent = Nothing, top = False}), top = False}",
         "Symbol {name = y Just line 1 column 17, ty = a, parent = Just (Symbol {name = y Just line 1 column 9, ty = Num a => a, parent = Nothing, top = False}), top = False}"
         ]

      it "pattern matching" $ "let f x = match x with y -> y" --> [
         "Symbol {name = f Just line 1 column 5, ty = a -> a, parent = Nothing, top = True}",
         "Symbol {name = x Just line 1 column 7, ty = a, parent = Nothing, top = False}",
         "Symbol {name = x Just line 1 column 17, ty = a, parent = Just (Symbol {name = x Just line 1 column 7, ty = a, parent = Nothing, top = False}), top = False}",
         "Symbol {name = y Just line 1 column 24, ty = a, parent = Nothing, top = False}",
         "Symbol {name = y Just line 1 column 29, ty = a, parent = Just (Symbol {name = y Just line 1 column 24, ty = a, parent = Nothing, top = False}), top = False}"
       ]