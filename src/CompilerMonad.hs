module CompilerMonad where

import Location (PString)
import Monads ( ErrorReaderWriterState )
import Environment ( Env ) 
import ContextReduction ( ClassEnv )
import Types ( Type )
import Data.Map ( Map )

newtype SymbolTable = SymbolTable (Map String Type)

type CompileM = ErrorReaderWriterState PString (ClassEnv, Env) [String] [SymbolTable]