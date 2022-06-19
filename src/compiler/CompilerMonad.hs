module CompilerMonad where

import Location (PString)
import Environment ( Env )
import ContextReduction ( ClassEnv )
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.RWS.Lazy as S
import SymbolTable ( Symbol )
import InterpreterMonad (InterpreterEnv)
import Ast (TypeDecl)

type CompilerState = (Env, [Symbol], [TypeDecl]) 

type CompileM = E.ExceptT PString (S.RWST (String, InterpreterEnv, ClassEnv) [String] CompilerState IO)

run :: CompileM a -> (String, InterpreterEnv, ClassEnv) -> CompilerState -> IO (Either PString a, CompilerState, [String])
run m  = S.runRWST (E.runExceptT m)
