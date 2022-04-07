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
import qualified CompilerTests
import qualified MandelbrotTests
import qualified CompileToJsTests
import qualified JavaScriptRunnerTests
import qualified ClosureConversionTests
import qualified CompileToClosedJsTests
import qualified CompileToCloseANFdJsTests
import qualified OptimizeTypeClassesTests

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
    CompilerTests.tests
    MandelbrotTests.tests
    CompileToJsTests.tests 
    JavaScriptRunnerTests.tests
    ClosureConversionTests.tests
    CompileToClosedJsTests.tests
    CompileToCloseANFdJsTests.tests
    OptimizeTypeClassesTests.tests
    
