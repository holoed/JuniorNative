module CompilerMonad where

import Location (PString)
import Environment ( Env )
import ContextReduction ( ClassEnv )
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.RWS.Lazy as S
import SymbolTable ( Symbol )

type CompilerState = (Env, [Symbol]) 

type CompileM = E.ExceptT PString (S.RWST ClassEnv [String] CompilerState IO)

run :: CompileM a -> ClassEnv -> CompilerState -> IO (Either PString a, CompilerState, [String])
run m  = S.runRWST (E.runExceptT m)
