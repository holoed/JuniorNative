{-# LANGUAGE QuasiQuotes #-}
module DagBindingsTests where

import Parser (parseExpr)
import DagBindings ( create )
import SynExpToExp ( toExp )
import Data.String.Interpolate ( i )
import Test.Hspec ( it, describe, shouldBe, SpecWith, Expectation )

build :: String -> [(String, [String])]
build code = create $ toExp <$> either error id (parseExpr code)

(-->) :: String -> [(String, [String])] -> Expectation
(-->) x y = build x `shouldBe` y

tests :: SpecWith ()
tests =
  describe "Build graph" $ do
      
   it "No deps" $ "let x = 42" --> [("x", [])]

   it "One dep" $ "let foo = fac 5" --> [("foo", ["fac"])]

   it "Many deps" $ [i|let quicksort f xs =
                        if (null xs) then xs else  
                        let lessThan = filter (\\x -> f x < f (hd xs)) (tl xs) in 
                        let greaterThan = filter (\\x -> f x > f (hd xs)) (tl xs) in
                        (quickSort f lessThan) ++ singleton (hd xs) ++ (quickSort f greaterThan)
                    |] --> [("quicksort",["++","<",">","filter","hd","null","quickSort","singleton","tl"])]
                       


