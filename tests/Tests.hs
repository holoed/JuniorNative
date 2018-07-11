module Main where

import Test.Hspec
import qualified TypeInferenceTests
import qualified AnnotationsTests
import qualified ClosureConversionTests


main :: IO ()
main = hspec $ do
    TypeInferenceTests.tests
    AnnotationsTests.tests
    ClosureConversionTests.tests
