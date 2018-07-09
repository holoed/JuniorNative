module Main where

import Test.Hspec
import qualified TypeInferenceTests
import qualified ClosureConversionTests


main :: IO ()
main = hspec $ do
    TypeInferenceTests.tests
    ClosureConversionTests.tests
