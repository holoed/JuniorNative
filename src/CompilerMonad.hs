module CompilerMonad where

import Location (PString)
import Environment ( Env )
import ContextReduction ( ClassEnv )
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.RWS.Lazy as S

type CompileM = E.ExceptT PString (S.RWST ClassEnv [String] Env IO)

run :: CompileM a -> ClassEnv -> Env -> IO (Either PString a, Env, [String])
run m  = S.runRWST (E.runExceptT m)
