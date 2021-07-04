module CompilerMonad where

import Location (PString)
import Environment ( Env )
import ContextReduction ( ClassEnv )
import Types ( Type )
import Data.Map ( Map )
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.RWS.Lazy as S

newtype SymbolTable = SymbolTable (Map String Type)

type CompileM = E.ExceptT PString (S.RWST (ClassEnv, Env) [String] [SymbolTable] IO)

run :: CompileM a -> (ClassEnv, Env) -> [SymbolTable] -> IO (Either PString a, [SymbolTable], [String])
run m  = S.runRWST (E.runExceptT m)
