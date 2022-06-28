{-# LANGUAGE QuasiQuotes #-}
module DesugarPatternMatchingSpec where

import Data.String.Interpolate ( i )
import Test.Hspec ( describe, it, shouldBe, Spec, parallel, Expectation )
import Intrinsics (classEnv, env)
import InterpreterMonad (empty)
import CompilerMonad (run, CompileM)
import TypedAst (TypedExp)
import SynExpToExp ( fromExp )
import PrettyPrinter (prettyPrint)
import Annotations (mapAnn)
import Data.Char (isSpace)
import CompilerSteps (desugarPatternMatching)
import Compiler (step, frontEnd)
import Control.Monad ((>=>))

closed :: String -> CompileM [TypedExp]
closed = frontEnd >=> step "desugar pattern matching" desugarPatternMatching  

process :: String -> IO [String]
process code = do
    (ret, _, _) <- run (closed code) ("", empty) (classEnv, env, [], [])
    return $ either (error . show) (toString <$>) ret

toString :: TypedExp -> String
toString = prettyPrint . fromExp . mapAnn fst

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

(-->) :: String -> String -> Expectation
(-->) x y = (filter (\v -> (/=' ') v && (/='\n') v) x) `shouldBe` (filter (\v -> (/=' ') v && (/='\n') v) y) 

spec :: Spec
spec = parallel $
  describe "Desugar Pattern Matching Tests" $ do

    it "pattern match 0" $ do
      xs <- process "let foo x = match x with y -> y"
      unlines xs --> "let foo x = (matchFn (((\\y -> True, \\y -> y)) : [])) x"
    
    it "pattern match 1" $ do
      let code = [i|
           data Option a = None | Some a
           let foo x = match x with None -> 42
         |]
      xs <- process code 
      unlines xs --> "let foo x = matchFn ((isNone, \\_v -> fromInteger 42) : []) x"

    it "pattern match 2" $ do
      let code = [i|
           data Option a = None | Some a
           let foo x = match x with Some v -> v + 1
         |]
      xs <- process code 
      unlines xs --> "let foo x = matchFn ((isSome, \\_v -> let v = extractSome _v in v + fromInteger 1 ) : []) x"

    it "pattern match 3" $ do
      let code = [i|
           data Option a = None | Some a
           let foo x = match x with Some None -> 42
         |]
      xs <- process code 
      unlines xs --> "let foo x = matchFn ((\\_v-> isSome _v && isNone(extractSome _v), \\_v -> fromInteger 42) : []) x"

    it "pattern match 4" $ do
      let code = [i|
           data ListF a b = Empty | Cons a b
           let swap v = 
            match v with
            | (Cons a Empty) -> Cons a Empty

         |]
      xs <- process code 
      unlines xs --> "let swap v = matchFn ((isCons, \\_v -> let (a,___w0) = extractCons _v in Cons a Empty) : []) v"
   