{-# LANGUAGE QuasiQuotes #-}
module DependencyAnalysisSpec where

import Parser (parseExpr)
import DependencyAnalysis ( deps, chunks )
import SynExpToExp ( toExp )
import Data.String.Interpolate ( i )
import Data.Set (Set, fromList)
import Test.Hspec ( it, describe, shouldBe, Spec )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )

globals :: Set [Char]
globals = fromList ["+", "-", "/", "*", "++", "==", "/=", ">", "<", "head", "tail", "null", "[]", ":", "&&", "||"]

spec :: Spec
spec = do
  describe "Build dependencies" $ do

   let build code = deps globals $ toExp <$> either (error . show) id (parseExpr code)
   let (-->) x y = build x `shouldBe` y

   it "No deps" $ "let x = 42" --> [("x", [])]

   it "One dep" $ "let foo = fac 5" --> [("foo", ["fac"])]

   it "Many deps" $ [i|let quicksort f xs =
                        if (null xs) then xs else  
                        let lessThan = filter (\\x -> f x < f (head xs)) (tail xs) in 
                        let greaterThan = filter (\\x -> f x > f (head xs)) (tail xs) in
                        (quicksort f lessThan) ++ singleton (head xs) ++ (quicksort f greaterThan)
                    |] --> [("quicksort",["filter", "singleton"])]

   it "Multi bindings deps" $ [i| let x = 12
                                  let y = 32
                                  let z = x + y |] --> [("z",["x","y"]),("y",[]),("x",[])]

  describe "Build chunks" $ do

    let build code = chunks globals $ toExp <$> either (error . show) id (parseExpr code)
    let (-->) x y = build x `shouldBe` y
    let (--->) x y = do handle <- openFile x ReadMode
                        contents <- hGetContents handle
                        contents --> y

    it "single node" $ "let x = 42" --> [[("x",[])]]

    it "two indipendent nodes" $ 
               [i| let x = 12
                   let y = 32 |] --> [[("x", []), ("y", [])]]

    it "two dependent nodes" $ 
               [i| let x = 12
                   let y = x + 1 |] --> [[("x", [])], [("y", ["x"])]]

    it "one node depending on two" $ 
               [i| let x = 12
                   let y = 32
                   let z = x + y |] --> [[("x", []), ("y", [])], [("z", ["x", "y"])]]

    it "Many nodes" $ [i|       
                      let foldr f v xs = 
                         if (null xs) then v 
                         else f (head xs) (foldr f v (tail xs)) 

                      let concat xs ys = foldr (\\x xs -> x : xs) ys
                     
                      let filter p = foldr (\\x -> \\xs -> if (p x) then x : xs else xs) []
                    
                      let singleton x = x : []

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let lessThan = filter (\\x -> f x < f (head xs)) (tail xs) in 
                        let greaterThan = filter (\\x -> f x > f (head xs)) (tail xs) in
                        concat (concat (quicksort f lessThan) (singleton (head xs))) (quicksort f greaterThan)
                      |] --> [
                        [("foldr",[]),("singleton",[])],
                        [("concat",["foldr"]),("filter",["foldr"])],
                        [("quicksort",["concat","filter","singleton"])]
                        ]
                       
    it "Complex example" $ "tests/jnrs_lib/example.jnr" ---> [          
          [("foldr",[]),("cadd",[]),("cmul",[]),("foldl",[]),("norm",[]),("posToCoord",[]),("singleton",[]),("range",[])],
          [("++",["foldr"]),("filter",["foldr"]),("map",["foldr"])],
          [("join",["foldl"]),("reverse",["foldl"]),("product",["foldl"]),("sum",["foldl"])],
          [("mPoint",["cadd","cmul","norm"])],
          [("mandelbrot",["cos","mPoint","toDouble","truncate"])],
          [("mapM",["bind","foldr","pure"])],
          [("partition",["reverse"])],
          [("quicksort",["filter","singleton"])],
          [("sequence",["mapM"])],
          [("split",["fst","partition","reverse","snd"])]
        ]

