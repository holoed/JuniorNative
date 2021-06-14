module Main where

import Test.Hspec ( hspec )
import qualified AlphaRenameTests
import qualified PrettyPrinterTests
import qualified TypeInferenceTests
import qualified AnnotationsTests
import qualified FreeVariablesTests


main :: IO ()
main = hspec $ do
    PrettyPrinterTests.tests
    AlphaRenameTests.tests
    TypeInferenceTests.tests
    AnnotationsTests.tests
    FreeVariablesTests.tests
