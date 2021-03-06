module Main where

import Test.Hspec ( hspec )
import qualified AlphaRenameTests
import qualified PrettyPrinterTests
import qualified TypeInferenceTests
import qualified AnnotationsTests
import qualified FreeVariablesTests
import qualified DependencyAnalysisTests
import qualified ModuleTypeInferenceTests
import qualified SymbolTableTests
import qualified PrettyTypesTests
import qualified ModulePrinterTests
import qualified InterpreterTests
import qualified ConstraintsResolutionTests

main :: IO ()
main = hspec $ do
    PrettyPrinterTests.tests
    AlphaRenameTests.tests
    TypeInferenceTests.tests
    AnnotationsTests.tests
    FreeVariablesTests.tests
    DependencyAnalysisTests.tests 
    ModuleTypeInferenceTests.tests 
    SymbolTableTests.tests
    PrettyTypesTests.tests
    ModulePrinterTests.tests
    InterpreterTests.tests
    ConstraintsResolutionTests.tests 
    
