module Junior.Compiler.CompilerMonad where

import Junior.Parser.Location (PString)
import Junior.TypeChecker.Environment ( Env )
import Junior.TypeChecker.ContextReduction ( ClassEnv )
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.RWS.Lazy as S
import Junior.Compiler.SymbolTable ( Symbol )
import Junior.Interpreter.InterpreterMonad (InterpreterEnv)
import Junior.Core.Ast (TypeDecl)

type CompilerState = (ClassEnv, Env, [Symbol], [TypeDecl]) 

type CompileM = E.ExceptT PString (S.RWST (String, InterpreterEnv) [String] CompilerState IO)

run :: CompileM a -> (String, InterpreterEnv) -> CompilerState -> IO (Either PString a, CompilerState, [String])
run m  = S.runRWST (E.runExceptT m)
