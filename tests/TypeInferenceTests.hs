{-# LANGUAGE QuasiQuotes #-}

module TypeInferenceTests where

import qualified Data.Set as Set
import Test.Hspec ( describe, it, shouldBe, SpecWith, Expectation )
import Types ( Type(..), Qual(..), Pred(..) )
import Environment ( Env, toEnv )
import SynExpToExp ( toExp )
import Infer (infer)
import Parser (parseExpr)
import Substitutions ( Substitutions )
import LiftNumbers ( liftN )

tyLam :: Type -> Type -> Type
tyLam t1 = TyApp (TyApp (TyCon "->") t1)

env :: Env
env = toEnv [("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
            ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
            ("filter", Set.fromList [] :=> tyLam (tyLam (TyVar "a" 0) (TyCon "Bool")) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("hd", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("singleton", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
            ("isEmpty", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("concat", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0))
     ]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

typeOf :: [String] -> Either String (Substitutions, Qual Type)
typeOf s = parseExpr (unlines s) >>= (infer classEnv env . liftN . toExp . head)

(-->) :: [String] -> String -> Expectation
(-->) x y = either id (show . snd) (typeOf x) `shouldBe` y

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
      ["3 - (2 / 3)"] --> "(Fractional a, Num a) => a"

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
      ["if \"5\" then True else False"] --> "Unable to unify String with Bool at line 1 column 4"

    it "type of tuple" $ do
      ["(2, True)"] --> "Num a => (a, Bool)"
      ["(False, 4)"] --> "Num a => (Bool, a)"
      ["\\x -> (x, x)"] --> "a -> (a, a)"
      ["\\x -> \\y -> (y, x)"] --> "a -> b -> (b, a)"

    it "type of let" $ do
      ["let x = 42 in x"] --> "Num a => a"
      ["let pair = (True, 12) in pair"] --> "Num a => (Bool, a)"
      ["let x = if (True) then 2 else 3 in x + 1"] --> "Num a => a"
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
      ["let foldr f z xs = if isEmpty xs then z else f (hd xs) (foldr f z (tl xs))"] --> "(a -> b -> b) -> b -> List a -> b"

    it "type of functions 6" $
      ["let f x = x in (f 5, f True)"] --> "Num a => (a, Bool)"

    it "type of functions 7" $
      -- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
      ["let f x = let g y = (x, y) in (g 3, g True) in f"] --> "Num a => b -> ((b, a), (b, Bool))"

    it "type of functions 7" $
      ["let map f xs = if isEmpty xs then empty else cons (f (hd xs)) (map f (tl xs))"] --> "(a -> b) -> List a -> List b"

    it "type of functions 8" $
      ["let qsort xs = if (isEmpty xs) then xs",
       "                  else concat (concat (qsort (filter (\\y -> y < hd xs) (tl xs)))",
       "                                      (singleton (hd xs)))",
       "                                      (qsort (filter (\\y -> y > hd xs) (tl xs))) in qsort"] --> "Ord a => List a -> List a"

    it "Apply function with wrong tuple arity" $ do
      ["let f x = (fst x, snd x) in f (1, 2, 3)"] --> "Unable to unify Tuple T12 with Tuple at line 1 column 31"

    it "Equality of tuples" $ do
      ["let f x y = (x, y) == (x, y) in f"] --> "(Eq a, Eq b) => a -> b -> Bool"

    it "Equality of tuples Error" $ do
      ["let f x y z = (x, y) == (x, y, z) in f"] --> "Unable to unify Tuple T15 with Tuple at line 1 column 25"  
