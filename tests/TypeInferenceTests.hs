{-# LANGUAGE QuasiQuotes #-}
module TypeInferenceTests where

import Fixpoint (Fix(..))
import Annotations (Ann(..))
import Location ( PString(..) )
import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation )
import Types ( Type(..), Qual(..), tyLam )
import SynExpToExp ( toExp )
import Infer (infer)
import Parser (parseExpr)
import Substitutions ( Substitutions )
import LiftNumbers ( liftN )
import qualified Data.Set as Set
import Intrinsics ( env, classEnv )
import Environment (Env, toEnv, concatEnvs)
import PrettyTypes (prettyQ)

env' :: Env
env' = concatEnvs env $ toEnv [
  ("concat", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("filter", Set.fromList [] :=> tyLam (tyLam (TyVar "a" 0) (TyCon "Bool")) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("singleton", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "List") (TyVar "a" 0)))    
 ]

typeOf :: [String] -> Either PString (Substitutions, Qual Type)
typeOf s = parseExpr (unlines s) >>=  
           (infer classEnv env' . liftN . toExp . head) >>=
           (\(subs, In(Ann (_, qt)  _)) -> pure (subs, prettyQ qt) )

(-->) :: [String] -> String -> Expectation
(-->) x y = either (\(PStr (s, _)) -> s) (show . snd) (typeOf x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Type Inference Tests" $ do

    it "type of a literal Int" $ ["42"] --> "Num a => a"
    it "type of a literal Double" $ ["2.5"] --> "Fractional a => a"
    it "type of a literal String" $ ["\"Hello\""] --> "String"
    it "type of a literal Bool" $ ["True"] --> "Bool"

    it "type of a lambda" $
      ["\\x y -> x + y"] --> "Num a => a -> a -> a"

    it "type of a lambda 2" $
      ["\\x -> \\y -> x y"] --> "(a -> b) -> a -> b"

    it "type of simple math" $
      ["12 + 24"] --> "Num a => a"

    it "type of simple math 2" $
      ["2 * (3 + 2)"] --> "Num a => a"

    it "type of simple math 3" $
      ["3 - (2 / 3)"] --> "Fractional a => a"

    it "type of simple class constraints" $ do
      ["2 > 3"] --> "Bool"
      ["\\x -> (x + x) < (x * x)"] --> "(Num a, Ord a) => a -> Bool"

    it "type of a name" $ do
      ["id"] --> "a -> a"
      ["foo"] --> "Name foo not found."

    it "type of identity" $
      ["\\x -> x"] --> "a -> a"

    it "type of nested lam that take 2 arg and return first" $
      ["\\x -> \\y -> x"] --> "a -> b -> a"

    it "type of composition" $
      ["\\f g x -> g (f x)"] --> "(a -> b) -> (b -> c) -> a -> c"

    it "type of fmap" $
      ["\\f m ctx -> f (m (ctx))"] --> "(a -> b) -> (c -> a) -> c -> b"

    it "type of applying identity to Int" $
      ["id 42"] --> "Num a => a"

    it "type of conditionals" $ do
      ["if True then 5 else 6"] --> "Num a => a"
      ["if True then 5 else False"] --> "Cannot find class instance for Num Bool"
      ["if True then True else 5"] -->  "Cannot find class instance for Num Bool"
      ["if 5 then True else False"] --> "Cannot find class instance for Num Bool"
      ["if \"5\" then True else False"] --> "Unable to unify String with Bool"

    it "type of tuple" $ do
      ["(2, True)"] --> "Num a => (a, Bool)"
      ["(False, 4)"] --> "Num a => (Bool, a)"
      ["\\x -> (x, x)"] --> "a -> (a, a)"
      ["\\x -> \\y -> (y, x)"] --> "a -> b -> (b, a)"

    it "type of let" $ do
      ["let x = 42 in x"] --> "Int"
      ["let pair = (True, 12) in pair"] --> "(Bool, Int)"
      ["let x = if (True) then 2 else 3 in x + 1"] --> "Int"
      ["let foo = \\x -> x + x in foo"] --> "Num a => a -> a"
      ["let foo x = x + x in foo"] --> "Num a => a -> a"

    it "type of functions 1" $
      ["let f = \\x -> x in f"] --> "a -> a"

    it "type of functions 2" $
      ["let swap = \\p -> (snd p, fst p) in swap"] --> "(a, b) -> (b, a)"

    it "type of functions 3" $
      ["let fix = \\f -> f (fix f) in fix"] --> "(a -> a) -> a"

    it "type of functions 4" $
      ["let fac = \\n -> if (n == 0) then 1 else n * (fac (n - 1)) in fac"] --> "(Eq a, Num a) => a -> a"

    it "type of functions 5" $
      ["let fib n = if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2) "] --> "(Eq a, Num a, Num b) => a -> b"

    it "type of functions 6" $
      ["let foldr f z xs = if null xs then z else f (head xs) (foldr f z (tail xs))"] --> "(a -> b -> b) -> b -> List a -> b"

    it "type of functions 7" $
      ["let f x = x in (f 5, f True)"] --> "Num a => (a, Bool)"

    -- it "type of functions 8" $
    --   -- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
    --   ["let f x = let g y = (x, y) in (g 3, g True) in f"] --> "Num a => b -> ((b, a), (b, Bool))"

    it "type of functions 9" $
      ["let map f xs = if null xs then [] else (f (head xs)) : (map f (tail xs))"] --> "(a -> b) -> List a -> List b"

    it "type of functions 10" $
      ["let qsort xs = if (null xs) then xs",
       "                  else concat (concat (qsort (filter (\\y -> y < head xs) (tail xs)))",
       "                                      (singleton (head xs)))",
       "                                      (qsort (filter (\\y -> y > head xs) (tail xs))) in qsort"] --> "Ord a => List a -> List a"

    it "Apply function with wrong tuple arity" $ do
      ["let f x = (fst x, snd x) in f (1, 2, 3)"] --> "Unable to unify Tuple T11 with Tuple"

    it "Equality of tuples" $ do
      ["let f x y = (x, y) == (x, y) in f"] --> "(Eq a, Eq b) => a -> b -> Bool"

    it "Nested Equality of tuples" $ do
      ["let f x y z = ((x, y), z) == ((x, y), z) in f"] --> "(Eq a, Eq b, Eq c) => b -> c -> a -> Bool"

    it "Equality of tuples Error" $ do
      ["let f x y z = (x, y) == (x, y, z) in f"] --> "Unable to unify Tuple T12 with Tuple"

    it "Tuple pattern in lambda" $ do
      ["\\(x, y) -> (y, x)"] --> "(a, b) -> (b, a)"
      ["\\(x, y, z) -> (y, x, z)"] --> "(a, b, c) -> (b, a, c)"
      ["\\(x1, x2, x3, x4) -> (x3, x1, x4, x2)"] --> "(a, b, c, d) -> (c, a, d, b)"
      ["\\(x1, x2, x3, x4, x5) -> (x2, x1, x5, x4, x3)"] --> "(a, b, c, d, e) -> (b, a, e, d, c)"

    it "Tuple pattern in let function" $ do
      ["let f (x, y) = (y, x)"] --> "(a, b) -> (b, a)"
      ["let f (x, y, z) = (y, x, z)"] --> "(a, b, c) -> (b, a, c)"

    it "Composition example" $
      ["let f = toDouble . truncate"] --> "Double -> Double"

    it "Simplify predicate list according to class env inheritance" $
      ["let f x y = x == y && x > y"] --> "Ord a => a -> a -> Bool"

    it "Type symbolic functions" $
      ["let (++) x y = (x, y) in 2 ++ 3"] --> "(Num a, Num b) => (b, a)"

    it "HOG Test 1" $
      ["fmap (\\x -> x + 1) (0:[])"] --> "Num a => List a"

    it "HOG Test 2" $
      ["fmap cos (pure 5)"] --> "(Applicative a, Floating b) => a b"

    it "HOG Test 3" $
      ["bind (0:[]) pure"] --> "Num a => List a"

    it "HOG Test 4" $
      ["runReader (pure 5)"] --> "Num a => b -> a"

    it "HOG Test 5" $
      ["runReader (pure 5) 6"] --> "Num a => a"

    it "Composition" $
      ["let (.) f g x = f (g x)"] --> "(a -> b) -> (c -> a) -> c -> b"

    it "scoped tvars" $
      ["let f x = let y = (x, x, x) in (y, x)"] --> "a -> ((a, a, a), a)"
