{-# LANGUAGE QuasiQuotes #-}

module TypeInferenceTests where

import qualified Data.Set as Set
import Test.Hspec
import Types
import Environment
import SynExpToExp
import Infer (infer)
import Parser (parseExpr)
import Substitutions
import LiftNumbers
import Data.String.Interpolate (i)

env :: Env
env = toEnv [("id", Set.fromList [] :=> TyLam (TyVar "a" 0) (TyVar "a" 0)),
            ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyVar "a" 0))),
            (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> TyLam (TyVar "a" 0) (TyLam (TyVar "a" 0) (TyCon "Bool"))),
            ("fst", Set.fromList [] :=> TyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
            ("snd", Set.fromList [] :=> TyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
            ("filter", Set.fromList [] :=> TyLam (TyLam (TyVar "a" 0) (TyCon "Bool")) (TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("hd", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
            ("tl", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("singleton", Set.fromList [] :=> TyLam (TyVar "a" 0) (TyApp (TyCon "List") (TyVar "a" 0))),
            ("isEmpty", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
            ("concat", Set.fromList [] :=> TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
            ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> TyLam (TyCon "Int") (TyVar "a" 0)),
            ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> TyLam (TyCon "Double") (TyVar "a" 0))
     ]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]

typeOf :: String -> Either String (Substitutions, Qual Type)
typeOf s = parseExpr s >>= (infer classEnv env . liftN . toExp)

(-->) :: String -> String -> Expectation
(-->) x y = either id (show . snd) (typeOf x) `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Type Inference Tests" $ do

    it "type of a literal" $ do
      [i|42|] --> "Num a => a"
      [i|"Hello"|] --> "String"

    it "type of a lambda" $ do
      [i|\\x y -> x + y|] --> "Num a => (a -> (a -> a))"

    it "type of simple math" $ do
      [i|12 + 24|] --> "Num a => a"
      [i|2 * (3 + 2)|] --> "Num a => a"
      [i|3 - (2 / 3)|] --> "(Fractional a, Num a) => a"

    it "type of simple class constraints" $ do 
      [i|2 > 3|] --> "Bool"
      [i|\\x -> (x + x) < (x * x)|] --> "(Num a, Ord a) => (a -> Bool)"

    it "type of a name" $ do
      [i|id|] --> "(a -> a)"
      [i|foo|] --> "Name foo not found."

    it "type of identity" $
      [i|\\x -> x|] --> "(a -> a)"

    it "type of nested lam that take 2 arg and return first" $
      [i|\\x -> \\y -> x|] --> "(a -> (b -> a))"

    it "type of composition" $
      [i|\\f g x -> g (f x)|] --> "((a -> b) -> ((b -> c) -> (a -> c)))"

    it "type of fmap" $
      [i|\\f m ctx -> f (m (ctx))|] --> "((a -> b) -> ((c -> a) -> (c -> b)))"

    it "type of applying identity to Int" $
      [i|id 42|] --> "Num a => a"

    it "type of conditionals" $ do
      [i|if True then 5 else 6|] --> "Num a => a"
      [i|if True then 5 else False|] --> "Cannot find class instance for Num Bool"
      [i|if True then True else 5|] -->  "Cannot find class instance for Num Bool"
      [i|if 5 then True else False|] --> "Cannot find class instance for Num Bool"
      [i|if \"5\" then True else False|] --> "Unable to unify String with Bool"

    it "type of tuple" $ do
      [i|(2, True)|] --> "Num a => (a, Bool)"
      [i|(False, 4)|] --> "Num a => (Bool, a)"
      [i|\\x -> (x, x)|] --> "(a -> (a, a))"
      [i|\\x -> \\y -> (y, x)|] --> "(a -> (b -> (b, a)))"

    it "type of let" $ do
      [i|let x = 42 in x|] --> "Num a => a"
      [i|let pair = (True, 12) in pair|] --> "Num a => (Bool, a)"
      [i|let x = if (True) then 2 else 3 in x + 1|] --> "Num a => a"
      [i|let foo = \\x -> x + x in foo|] --> "Num a => (a -> a)"
      [i|let foo x = x + x in foo|] --> "Num a => (a -> a)"

    it "type of functions" $ do
      [i|let f = \\x -> x in f|] --> "(a -> a)"
      [i|let swap = \\p -> (snd p, fst p) in swap|] --> "((a, b) -> (b, a))"
      [i|let fix = \\f -> f (fix f) in fix|] --> "((a -> a) -> a)"
      [i|let fac = \\n -> if (n == 0) then 1 else n * (fac (n - 1)) in fac|] --> "(Eq a, Num a) => (a -> a)"
      [i|let f x = x in (f 5, f True)|] --> "Num a => (a, Bool)"
      -- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
      [i|let f x = let g y = (x, y) in (g 3, g True) in f|] --> "Num a => (b -> ((b, a), (b, Bool)))"
      [i|let qsort xs = if (isEmpty xs) then xs 
                        else concat (concat (qsort (filter (\\y -> y < hd xs) (tl xs))) 
                                            (singleton (hd xs))) 
                                            (qsort (filter (\\y -> y > hd xs) (tl xs))) in qsort|] --> "Ord a => (List a -> List a)"

    it "Apply function with wrong tuple arity" $ do
      [i|let f x = (fst x, snd x) in f (1, 2, 3)|] --> "Unable to unify Tuple T11 with Tuple"

    it "Equality of tuples" $ do
      [i|let f x y = (x, y) == (x, y) in f|] --> "(Eq a, Eq b) => (a -> (b -> Bool))"
