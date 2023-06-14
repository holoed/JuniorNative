module UnitTests.AlphaRenameSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import Junior.Pretty.Printer ( prettyPrint )
import Junior.Parser.SynExpToExp (fromExp)
import Control.Monad.Catch (MonadThrow)
import Junior.Compiler.CompilerMonad (CompileM, run)
import Junior.Interpreter.InterpreterMonad (empty)
import Junior.Compiler.Intrinsics (classEnv, env)
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Compiler.Compiler (frontEnd, step)
import Control.Monad ((>=>))
import Junior.Compiler.CompilerSteps (renameVars)
import Control.Monad.Cont (MonadIO(liftIO))
import Junior.Utils.Annotations (mapAnn)

closed :: String -> CompileM [TypedExp]
closed = frontEnd >=> step "Rename" renameVars

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) ("", empty) (classEnv, env, [], [])
    return $ either (error . show) (toString <$>) ret

(-->) :: (MonadIO m, MonadThrow m) => String -> String -> m ()
(-->) x y = do v <- liftIO $ process x
               v `shouldBe` [y]

tests :: TopSpec
tests = parallel $
  describe "Alpha Rename Tests" $ do

     it "Rename identity" $
       "let id x = x" --> "let id x00 = x00"

     it "Rename simple lambda shadowing" $
       "let f = \\x -> \\x -> x" --> "let f x00 x11 = x11"

     it "Rename curry function" $
       "let g = \\f -> \\x -> \\y -> f (x, y)" --> "let g f00 x11 y22 = f00 (x11, y22)"

     it "Rename a tuple" $
       "let g = \\x -> \\y -> (x, y)" --> "let g x00 y11 = (x00, y11)"

     it "Rename a let" $ do
       "let main = let x = 4 in x" --> "let main = let x00 = fromInteger 4 in x00"
       "let main = let f = \\x -> x in f 5" --> "let main = let f00 x11 = x11 in\n           f00 (fromInteger 5)"

     it "Rename match" $ do
        "let f x = match x with x -> x" --> "let f x00 = match x00 with x11 -> x11"

     it "Rename nested lets" $
       "let main = let x = 3 in let x = 4 in x" --> "let main = let x00 = fromInteger 3 in\n           let x11 = fromInteger 4 in x11"
  
     it "Rename nested lambdas" $
       "let f x = (\\x -> (\\x -> x)) x x" --> "let f x00 = ((\\x11 x22 -> x22) x00) x00"
  
     it "Rename function with pattern matching on tuples" $
       "let f (x, y) = x + y" --> "let f (x00, y11) = x00 + y11"

     it "Rename function with nested tuples" $ do
       "let f ((x, y), z) = x + y + z" --> "let f ((x00, y11), z22) = x00 + y11 + z22" 

     it "Rename function with multiple arguments" $ do
       "let f x y z = x + y + z" --> "let f x00 y11 z22 = x00 + y11 + z22" 

     it "Rename recursive function" $ do
       "let f x = if x == 0 then 1 else x * f (x - 1)" --> "let f x00 = if x00 == fromInteger 0\n    then fromInteger 1\n    else x00 * f (x00 - fromInteger 1)" 

     it "Rename function with pattern matching on constructors" $ do
       "let f (Just x) = x" --> "let f Just x00 = x00" -- TODO: fix pretty printer
       "let f Nothing = 0" --> "let f Nothing  = fromInteger 0" 

     it "Rename function with shadowed variables in let expressions" $ do
       "let f x = let x = 4 in x" --> "let f x00 = let x11 = fromInteger 4 in x11"
       "let g x = let y = 3 in let x = 4 in x + y" --> "let g x00 = let y11 = fromInteger 3 in\n            let x22 = fromInteger 4 in x22 + y11" 
 

  


