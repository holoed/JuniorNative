{-# LANGUAGE QuasiQuotes #-}
module ModuleTypeInferenceTests where

import Modules (typeOfModule)
import Data.String.Interpolate ( i )
import Test.Hspec ( SpecWith, describe, it, shouldBe, Expectation )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Intrinsics ( env, classEnv )


(-->) :: String -> [(String, String)] -> Expectation
(-->) x y = typeOfModule classEnv env x `shouldBe` y

(--->) :: FilePath -> [(String, String)] -> Expectation 
(--->) x y = do handle <- openFile x ReadMode
                contents <- hGetContents handle
                contents --> y

tests :: SpecWith ()
tests =
  describe "Module Type Inference Tests" $ do

    it "Simple bindings" $
           [i| let x = 12
               let y = True |] --> [("x","Num a => a"),
                                    ("y","Bool")]

    it "Dependent bindings" $
           [i| let x = 12
               let y = (x + 1, True) |] --> [("x","Num a => a"),
                                             ("y","Num a => (a, Bool)")]

    it "Many nodes" $ [i|       
                      let foldr f v xs = 
                         if (null xs) then v 
                         else f (hd xs) (foldr f v (tl xs)) 

                      let concat xs ys = foldr cons ys xs
                     
                      let filter p = foldr (\\x -> \\xs -> if (p x) then cons x xs else xs) empty
                    
                      let singleton x = cons x empty

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let pivot = hd xs in
                        let rest = tl xs in
                        let lessThan = filter (\\x -> f x < f pivot) rest in 
                        let greaterThan = filter (\\x -> f x > f pivot) rest in
                        concat (concat (quicksort f lessThan) (singleton pivot)) (quicksort f greaterThan)
                      |] --> [("foldr","(a -> b -> b) -> b -> List a -> b"),
                              ("concat","List a -> List a -> List a"),
                              ("filter","(a -> Bool) -> List a -> List a"),
                              ("singleton","a -> List a"),
                              ("quicksort","Ord a => (b -> a) -> List b -> List b")]

    it "Complex example" $ "tests/example.jnr" ---> [
       ("foldl","(a -> b -> a) -> a -> List b -> a"),
       ("join","List List a -> List a"),
       ("map","(a -> b) -> List a -> List b"),
       ("bind","(a -> List b) -> List a -> List b"),
       ("cadd","(Num a, Num b) => (a, b) -> (a, b) -> (a, b)"),
       ("cmul","Num a => (a, a) -> (a, a) -> (a, a)"),
       ("concat","List a -> List a -> List a"),
       ("filter","(a -> Bool) -> List a -> List a"),
       ("foldr","(a -> b -> b) -> b -> List a -> b"),
       ("norm","Num a => (a, a) -> a"),
       ("mPoint","(Num a, Num b, Ord a) => b -> (a, a) -> (a, a) -> b"),
       ("mandelbrot","Int -> (Int, Int) -> (Int, Int, Int)"),
       ("reverse","List a -> List a"),
       ("partition","Num a => a -> List b -> (List b, List b)"),
       ("posToCoord","Fractional a => a -> a -> (a, a)"),
       ("product","Num a => List a -> a"),
       ("singleton","a -> List a"),
       ("quicksort","Ord a => (b -> a) -> List b -> List b"),
       ("range","(Num a, Ord a) => (a -> b) -> a -> a -> List b"),
       ("split","Num a => a -> List b -> List List b"),
       ("sum","Num a => List a -> a")
     ]


