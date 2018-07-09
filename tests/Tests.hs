module Main where

import Test.Hspec
import qualified TypeInferenceTests


main :: IO ()
main = hspec 
    TypeInferenceTests.tests
