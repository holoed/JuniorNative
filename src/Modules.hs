module Modules where

import Location ( PString )
import Environment ( Env )
import ContextReduction ( ClassEnv ) 
import Compiler (pipeline)
import Monads (run)

typeOfModule :: ClassEnv -> Env -> String -> Either PString [(String, String)]
typeOfModule classEnv env x = 
   (\(x, _, _) -> x) <$> run (pipeline x) (classEnv, env) []
  
  