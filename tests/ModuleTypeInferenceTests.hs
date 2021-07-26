{-# LANGUAGE QuasiQuotes #-}
module ModuleTypeInferenceTests where

import Data.String.Interpolate ( i )
import Test.Hspec ( SpecWith, describe, it, shouldBe, Expectation )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Intrinsics ( env, classEnv )
import Location ( PString, getName )
import Compiler (frontEndPrinted)
import CompilerMonad (run)
import qualified SymbolTable as S
import Data.List (nub)
import PrettyTypes (prettyQ)
import qualified Data.HashMap as Map (empty) 

extractNames :: [S.Symbol] -> [(String, String)]
extractNames ss = (\s -> (getName $ S.name s, show $ prettyQ $ S.ty s)) <$> filter S.top ss

typeOfModule :: String -> IO (Either PString [(String, String)])
typeOfModule code = do
   (x, (_, ss), _) <- run (frontEndPrinted code) (Map.empty, classEnv) (env, [])
   return (nub . extractNames . const ss <$> x)

(-->) :: String -> [(String, String)] -> Expectation
(-->) x y = do v <- typeOfModule x
               either (error . show) id v `shouldBe` y

(--->) :: FilePath -> [(String, String)] -> Expectation
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

tests :: SpecWith ()
tests =
  describe "Module Type Inference Tests" $ do

    it "Simple bindings" $
           [i| let x = 12
               let y = True |] --> [("x","Int"),
                                    ("y","Bool")]

    it "Dependent bindings" $
           [i| let x = 12
               let y = (x + 1, True) |] --> [("x","Int"),
                                             ("y","(Int, Bool)")]

    it "Many nodes" $ [i|       
                      let foldr f v xs = 
                         if (null xs) then v 
                         else f (head xs) (foldr f v (tail xs)) 

                      let concat xs ys = foldr (\\x xs -> x : xs) ys xs
                     
                      let filter p = foldr (\\x -> \\xs -> if (p x) then x : xs else xs) []
                    
                      let singleton x = x : []

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let pivot = head xs in
                        let rest = tail xs in
                        let lessThan = filter (\\x -> f x < f pivot) rest in 
                        let greaterThan = filter (\\x -> f x > f pivot) rest in
                        concat (concat (quicksort f lessThan) (singleton pivot)) (quicksort f greaterThan)
                      |] -->
                       [
                          ("foldr","(a -> b -> b) -> b -> List a -> b"),
                          ("singleton","a -> List a"),
                          ("concat","List a -> List a -> List a"),
                          ("filter","(a -> Bool) -> List a -> List a"),
                          ("quicksort","Ord a => (b -> a) -> List b -> List b")
                       ]

    it "Complex example" $ "tests/jnrs_lib/example.jnr" ---> [
      ("foldr","(a -> b -> b) -> b -> List a -> b"),
      ("cadd","(Num a, Num b) => (a, b) -> (a, b) -> (a, b)"),
      ("cmul","Num a => (a, a) -> (a, a) -> (a, a)"),
      ("foldl","(a -> b -> a) -> a -> List b -> a"),
      ("norm","Num a => (a, a) -> a"),
      ("posToCoord","Fractional a => a -> a -> (a, a)"),
      ("singleton","a -> List a"),
      ("range","(Num a, Ord a) => (a -> b) -> a -> a -> List b"),
      ("++","List a -> List a -> List a"),
      ("filter","(a -> Bool) -> List a -> List a"),
      ("map","(a -> b) -> List a -> List b"),
      ("mapM","Monad a => (b -> a c) -> List b -> a List c"),
      ("join","List List a -> List a"),
      ("mPoint","(Eq a, Num a, Num b, Ord b) => a -> (b, b) -> (b, b) -> a"),
      ("mandelbrot","Int -> (Int, Int) -> (Int, Int, Int)"),
      ("reverse","List a -> List a"),
      ("partition","(Eq a, Num a) => a -> List b -> (List b, List b)"),
      ("product","Num a => List a -> a"),
      ("quicksort","Ord a => (b -> a) -> List b -> List b"),
      ("sequence","Monad a => List a b -> a List b"),
      ("split","(Eq a, Num a) => a -> List b -> List List b"),
      ("sum","Num a => List a -> a")
     ]

    it "Recursion schemes" $ "tests/jnrs_lib/RecursionSchemes.jnr" ---> [
      (".","(a -> b) -> (c -> a) -> c -> b"),
      ("ana","Functor a => (b -> a c) -> (c -> Fix a) -> b -> Fix a"),
      ("fix","((a -> b) -> a -> b) -> a -> b"),
      ("cata","Functor a => (a b -> c) -> (Fix a -> b) -> Fix a -> c"),
      ("hylo","Functor a => (b -> a c) -> (a d -> e) -> (c -> d) -> b -> e"),
      ("para","Functor a => (a (Fix a, b) -> c) -> (Fix a -> b) -> Fix a -> c"),
      ("anaRec","Functor a => (b -> a b) -> b -> Fix a"),
      ("cataRec","Functor a => (a b -> b) -> Fix a -> b"),
      ("hyloRec","Functor a => (b -> a b) -> (a c -> c) -> b -> c"),
      ("paraRec","Functor a => (a (Fix a, b) -> b) -> Fix a -> b")
     ]


