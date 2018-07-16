module Main where

import Test.Hspec
import qualified TypeInferenceTests
import qualified AnnotationsTests
import qualified FreeVariablesTests
import qualified ClosureConversionTests


main :: IO ()
main = hspec $ do
    TypeInferenceTests.tests
    AnnotationsTests.tests
    FreeVariablesTests.tests
    ClosureConversionTests.tests
