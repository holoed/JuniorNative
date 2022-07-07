{-# LANGUAGE QuasiQuotes #-}
module UnitTests.ModulePrinterSpec where

import Test.Sandwich (TopSpec, shouldBe, describe, it, parallel)
import Compiler ( frontEndPrinted )
import CompilerMonad ( run )
import Intrinsics ( env, classEnv )
import Data.String.Interpolate ( i )
import InterpreterMonad (empty) 
import Data.Text (unpack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Catch (MonadThrow)

build :: String -> IO String
build code = do
   (Right x, _, _) <- run (frontEndPrinted code) ("main", empty) (classEnv, env, [], [])
   return (unpack x)

(-->) :: (MonadIO m, MonadThrow m, MonadFail m) => String -> String -> m ()
(-->) s1 s2 = liftIO $ build s1 >>= (`shouldBe` s2)

tests :: TopSpec
tests = parallel $
  describe "Module Printer Tests" $ do

    it "Pretty one binding" $ do
        "let f x = x" --> [i|val f :: a -> a
let f x0 = x0
|]

    it "Pretty many binding" $ do
        [i|
let foldr f v xs =
  if (null xs) then v
  else f (head xs) (foldr f v (tail xs))|] --> [i|val foldr :: (a -> b -> b) -> b -> List a -> b
let foldr f0 v1 xs2 = if null xs2
    then v1
    else f0 (head xs2) (foldr f0 v1 (tail xs2))
|]