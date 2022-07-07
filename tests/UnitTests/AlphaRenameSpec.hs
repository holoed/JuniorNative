module UnitTests.AlphaRenameSpec where

import Test.Sandwich ( describe, it, shouldBe, TopSpec, parallel )
import PrettyPrinter ( prettyPrint )
import SynExpToExp (fromExp)
import Control.Monad.Catch (MonadThrow)
import CompilerMonad (CompileM, run)
import InterpreterMonad (empty)
import Intrinsics (classEnv, env)
import TypedAst (TypedExp)
import Compiler (frontEnd, step)
import Control.Monad ((>=>))
import CompilerSteps (renameVars)
import Control.Monad.Cont (MonadIO(liftIO))
import Annotations (mapAnn)

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
