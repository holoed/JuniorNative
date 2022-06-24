{-# LANGUAGE QuasiQuotes #-}
module TypeInferenceSpec where

import Data.String.Interpolate ( i )
import Fixpoint (Fix(..))
import Annotations (Ann(..))
import Location ( PString(..) )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation, parallel )
import Types ( Type(..), Qual(..), tyLam )
import SynExpToExp ( toExp )
import Infer (infer)
import Parser (parseExpr)
import LiftNumbers ( liftN )
import qualified Data.Set as Set
import Intrinsics ( env, classEnv )
import Environment (Env, toEnv, fromEnv, concatEnvs, containsScheme)
import PrettyTypes (prettyQ)
import PAst ( SynExp, SynExpF(VarPat, Defn) )
import Data.Bifunctor (second)
import Data.Map (toList, fromList, (!), member)
import Data.List (intercalate)
import Data.Maybe (fromJust)

env' :: Env
env' = concatEnvs env $ toEnv [
  (".",  Set.fromList [] :=> tyLam (tyLam (TyVar "b" 0) (TyVar "c" 0))
                             (tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0))
                             (tyLam (TyVar "a" 0) (TyVar "c" 0)))),
  ("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
  ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
  ("concat", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("filter", Set.fromList [] :=> tyLam (tyLam (TyVar "a" 0) (TyCon "Bool")) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("singleton", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "List") (TyVar "a" 0)))
 ]

extractName :: SynExp -> String
extractName (In (Ann _ (Defn _ (In (Ann _ (VarPat n)):_) _))) = n
extractName _ = "it"

typeOf :: String -> Either PString [(String, String)]
typeOf s = parseExpr s >>= typ . ((\e -> [(extractName e, (fromJust . toExp) e)]) <$>) >>=
           (\(_, env4) -> Right $ second (show . prettyQ) <$> fromEnv env4 )
    where
      g (_, env3) (n, e) = (\e2@(In (Ann (_, t) _)) -> ([e2], toEnv [(n, t)])) . snd <$> (infer classEnv env3 . liftN) e
      f env2 (n, e) = env2 >>= flip g (n, e)
      k ev1 ev2 = (\(e1, x) (e2, y) -> (e1 ++ e2, concatEnvs x y)) <$> ev1 <*> ev2
      h acc bs = foldl k acc (f acc <$> bs)
      typ bss = foldl h (Right ([], env')) bss

extractResults :: [(String, String)] -> String
extractResults = getKey . fromList . filter (\(n, _) -> not $ containsScheme n env')
  where getKey dict
          | member "it" dict = dict!"it"
          | member "main" dict = dict!"main"
          | otherwise = intercalate "\n" (snd <$> toList dict)

(-->) :: String -> String -> Expectation
(-->) x y = either (\(PStr (txt, _)) -> txt) extractResults (typeOf x) `shouldBe` y

spec :: Spec
spec = parallel $
  describe "Type Inference Tests" $ do

    it "Integral instances" $ do
      "let main = mod 5 3" --> "Int"

    it "type of a literal Int" $ "42" --> "Num a => a"
    it "type of a literal Double" $ "2.5" --> "Fractional a => a"
    it "type of a literal String" $ "\"Hello\"" --> "String"
    it "type of a literal Bool" $ "True" --> "Bool"

    it "type of a lambda" $
      "\\x y -> x + y" --> "Num a => a -> a -> a"

    it "type of a lambda 2" $
      "\\x -> \\y -> x y" --> "(a -> b) -> a -> b"

    it "type of simple math" $
      "12 + 24" --> "Num a => a"

    it "type of simple math 2" $
      "2 * (3 + 2)" --> "Num a => a"

    it "type of simple math 3" $
      "3 - (2 / 3)" --> "Fractional a => a"

    it "type of simple class constraints" $ do
      "2 > 3" --> "Bool"
      "\\x -> (x + x) < (x * x)" --> "(Num a, Ord a) => a -> Bool"

    it "type of a name" $ do
      "id" --> "a -> a"
      "foo" --> "Name foo not found."

    it "type of identity" $
      "\\x -> x" --> "a -> a"

    it "type of nested lam that take 2 arg and return first" $
      "\\x -> \\y -> x" --> "a -> b -> a"

    it "type of composition" $
      "\\f g x -> g (f x)" --> "(a -> b) -> (b -> c) -> a -> c"

    it "type of fmap" $
      "\\f m ctx -> f (m (ctx))" --> "(a -> b) -> (c -> a) -> c -> b"

    it "type of applying identity to Int" $
      "id 42" --> "Num a => a"

    it "type of conditionals" $ do
      "if True then 5 else 6" --> "Num a => a"
      "if True then 5 else False" --> "Cannot find class instance for Num Bool"
      "if True then True else 5" -->  "Cannot find class instance for Num Bool"
      "if 5 then True else False" --> "Cannot find class instance for Num Bool"
      "if \"5\" then True else False" --> "Unable to unify String with Bool"

    it "type of tuple" $ do
      "(2, True)" --> "Num a => (a, Bool)"
      "(False, 4)" --> "Num a => (Bool, a)"
      "\\x -> (x, x)" --> "a -> (a, a)"
      "\\x -> \\y -> (y, x)" --> "a -> b -> (b, a)"

    it "type of let" $ do
      "let x = 42" --> "Int"
      "let pair = (True, 12)" --> "(Bool, Int)"
      "let main = let x = if (True) then 2 else 3 in x + 1" --> "Int"
      "let foo = \\x -> x + x" --> "Num a => a -> a"
      "let foo x = x + x" --> "Num a => a -> a"

    it "type of functions 1" $
      "let f = \\x -> x" --> "a -> a"

    it "type of functions 2" $
      "let swap = \\p -> (snd p, fst p)" --> "(a, b) -> (b, a)"

    it "type of functions 3" $
      "let fix = \\f -> f (fix f)" --> "(a -> a) -> a"

    it "type of functions 4" $
      "let fac = \\n -> if (n == 0) then 1 else n * (fac (n - 1))" --> "(Eq a, Num a) => a -> a"

    it "type of functions 5" $
      "let fib n = if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2) " --> "(Eq a, Num a, Num b) => a -> b"

    it "type of functions 6" $
      "let foldright f z xs = if null xs then z else f (head xs) (foldright f z (tail xs))" --> "(a -> b -> b) -> b -> List a -> b"

    it "type of functions 7" $
      [i|let f x = x
         let main = (f 5, f True)|] --> "(Int, Bool)"

    -- it "type of functions 8" $
    --   -- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
    --   "let f x = let g y = (x, y) in (g 3, g True)" --> "Num a => b -> ((b, a), (b, Bool))"

    it "type of functions 9" $
      "let map f xs = if null xs then [] else (f (head xs)) : (map f (tail xs))" --> "(a -> b) -> List a -> List b"

    it "type of functions 10" $
      [i|let qsort xs = if (null xs) then xs
                          else concat (concat (qsort (filter (\\y -> y < head xs) (tail xs)))
                                              (singleton (head xs)))
                                              (qsort (filter (\\y -> y > head xs) (tail xs)))|] --> "Ord a => List a -> List a"

    it "Apply function with wrong tuple arity" $ do
      "let main = let f x = (fst x, snd x) in f (1, 2, 3)" --> "Unable to unify Tuple T11 with Tuple"

    it "Equality of tuples" $ do
      "let f x y = (x, y) == (x, y)" --> "(Eq a, Eq b) => a -> b -> Bool"

    it "Nested Equality of tuples" $ do
      "let f x y z = ((x, y), z) == ((x, y), z)" --> "(Eq a, Eq b, Eq c) => b -> c -> a -> Bool"

    it "Equality of tuples Error" $ do
      "let f x y z = (x, y) == (x, y, z)" --> "Unable to unify Tuple T12 with Tuple"

    it "Tuple pattern in lambda" $ do
      "\\(x, y) -> (y, x)" --> "(a, b) -> (b, a)"
      "\\(x, y, z) -> (y, x, z)" --> "(a, b, c) -> (b, a, c)"
      "\\(x1, x2, x3, x4) -> (x3, x1, x4, x2)" --> "(a, b, c, d) -> (c, a, d, b)"
      "\\(x1, x2, x3, x4, x5) -> (x2, x1, x5, x4, x3)" --> "(a, b, c, d, e) -> (b, a, e, d, c)"

    it "Tuple pattern in let function" $ do
      "let f (x, y) = (y, x)" --> "(a, b) -> (b, a)"
      "let f (x, y, z) = (y, x, z)" --> "(a, b, c) -> (b, a, c)"

    it "Composition example" $
      "let f = toDouble . truncate" --> "Double -> Double"

    it "Simplify predicate list according to class env inheritance" $
      "let f x y = x == y && x > y" --> "Ord a => a -> a -> Bool"

    it "Type symbolic functions" $
      "let main = let (++) x y = (x, y) in 2 ++ 3" --> "(Int, Int)"

    it "HOG Test 1" $
      "fmap (\\x -> x + 1) (0:[])" --> "Num a => List a"

    it "HOG Test 2" $
      "fmap cos (pure 5)" --> "(Applicative a, Floating b) => a b"

    it "HOG Test 3" $
      "(>>=) (0:[]) pure" --> "Num a => List a"

    it "HOG Test 4" $
      "runReader (pure 5)" --> "Num a => b -> a"

    it "HOG Test 5" $
      "runReader (pure 5) 6" --> "Num a => a"

    it "Composition" $
      "let compose f g x = f (g x)" --> "(a -> b) -> (c -> a) -> c -> b"

    it "scoped tvars" $
      "let f x = let y = (x, x, x) in (y, x)" --> "a -> ((a, a, a), a)"

    it "applicative operator" $ do
      "let main = (\\x -> x + 1):[] <*> 5:[]" --> "List Int"

    it "from list to map" $ do
      "let main = fromListToMap (('a', 2):[])" --> "Map Char Int"

    it "value signature" $ do
      [i|val main :: Double
         let main = 42|] --> "Double"
      [i|val main :: Num a => a
         let main = 42|] --> "Num a => a"

    it "function signature" $ do
      [i|val f :: Int -> Int
         let f x = x|] --> "Int -> Int"
    
    it "tuples in signatures" $ do
      [i|val pair :: (Int, Int)
         let pair = (2, 3)|] --> "(Int, Int)"
      [i|val norm :: (Double, Double) -> Double
         let norm (re, im) = re * re + im * im|] --> "(Double, Double) -> Double"

    it "type application signature" $ do
      [i|val f :: Int -> List Int
         let f x = x:[] |] --> "Int -> List Int"
      [i|val f :: Int -> List (List Int)
         let f x = (x:[]):[] |] --> "Int -> List (List Int)" 

    it "tuple deconstruction in nested lets" $ do
      [i|let add (z1, z2) = 
         let ((a, b), c) = z1 in
         let ((d, e), f) = z2 in
         ((a, d), (b, e), (c, f)) |] --> "(((a, b), c), ((d, e), f)) -> ((a, d), (b, e), (c, f))"

    it "13-type-checker-unification-bug" $ do
      [i|val f :: (a -> b) -> Maybe (List a) -> Maybe (List b)
         let f = fmap . fmap|] --> "(a -> b) -> Maybe (List a) -> Maybe (List b)"

    it "pattern matching 0" $ do
      "let foo x = match x with y -> y" --> "a -> a"

    it "pattern matching 1" $ do
      "let foo x = match x with (y, z) -> y" --> "(a, b) -> a"

    it "pattern matching 2" $ do
      [i|let foo x = match x with 
                   | (y, z) -> y
                   | (y, z) -> z|] --> "(a, a) -> a"

    it "pattern matching 3" $ do
      "let foo x = match x with Just y -> y" --> "Maybe a -> a"

    it "pattern matching 4" $ do
        [i|let foo x = match x with 
                   | Just x -> x + 1
                   | Nothing -> 0 |] --> "Num a => Maybe a -> a"

    it "pattern matching 5" $ do
        [i|let foo x = match x with 
               | Just (y, z) -> y + z 
               | Nothing -> 0  |] --> "Num a => Maybe (a, a) -> a"

    it "pattern matching 6" $ do
        [i|let foo x = match x with Just 4 -> 5 |] --> "Num a => Maybe Int -> a"



      

