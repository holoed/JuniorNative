{-# LANGUAGE QuasiQuotes #-}
module UnitTests.DependencyAnalysisSpec where

import Junior.Parser.Parser (parseExpr)
import Junior.Compiler.DependencyAnalysis ( deps, chunks )
import Junior.Parser.SynExpToExp ( toExp )
import Data.String.Interpolate ( i )
import Data.Set (Set, fromList)
import Test.Sandwich ( it, describe, shouldBe, TopSpec, parallel )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (MonadIO(liftIO))

globals :: Set [Char]
globals = fromList ["+", "-", "/", "*", "++", "!!", "==", "/=", ">", "<", "head", "tail", "null", "[]", ":", "&&", "||"]

tests :: TopSpec
tests = parallel $ do
  describe "Build dependencies" $ do

   let build code = deps globals $ fromJust . toExp <$> either (error . show) id (parseExpr code)
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

    let build code = chunks globals $ fromJust . toExp <$> either (error . show) id (parseExpr code)
    let (-->) x y = build x `shouldBe` y
    let (--->) x y = do handle <- liftIO $ openFile x ReadMode
                        contents <- liftIO $ hGetContents handle
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
                      let foldright f v xs = 
                         if (null xs) then v 
                         else f (head xs) (foldright f v (tail xs)) 

                      let concat xs ys = foldright (\\x xs -> x : xs) ys
                     
                      let filter p = foldright (\\x -> \\xs -> if (p x) then x : xs else xs) []
                    
                      let singleton x = x : []

                      let quicksort f xs =
                        if (null xs) then xs else  
                        let lessThan = filter (\\x -> f x < f (head xs)) (tail xs) in 
                        let greaterThan = filter (\\x -> f x > f (head xs)) (tail xs) in
                        concat (concat (quicksort f lessThan) (singleton (head xs))) (quicksort f greaterThan)
                      |] --> [
                        [("foldright",[]),("singleton",[])],
                        [("concat",["foldright"]),("filter",["foldright"])],
                        [("quicksort",["concat","filter","singleton"])]
                        ]
                       
    it "Complex example" $ "tests/jnrs_lib/example.jnr" ---> [          
          [("cadd", []), ("cmul", []), ("norm", []), ("posToCoord", []), ("singleton", []), ("range", [])], 
          [("++", ["foldr"]), ("filter", ["foldr"]), ("map", ["foldr"])], 
          [("join", ["foldl"]), ("reverse", ["foldl"]), ("product", ["foldl"]), ("sum", ["foldl"])], 
          [("mPoint", ["cadd", "cmul", "norm"])], 
          [("mandelbrot", ["cos", "mPoint", "toDouble", "truncate"])], 
          [("mapM", [">>=", "foldr", "pure"])], 
          [("partition", ["reverse"])], 
          [("quicksort", ["filter", "singleton"])], 
          [("sequence", ["mapM"])], 
          [("split", ["fst", "partition", "reverse", "snd"])]
        ]

    it "Reference to a binding defined later" $
          [i|let y = z
             let x = 1
             let z = x + y
           |] --> [[("x", [])], [("z", ["x", "y"])], [("y", ["z"])]]

    it "Multiple bindings and multiple references to previously defined bindings" $
          [i|let x = 1
             let y = x + 2
             let z = y * x
            |] --> [[("x", [])], [("y", ["x"])], [("z", ["x", "y"])]]



