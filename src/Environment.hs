module Environment where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
import Prelude hiding (lookup)

type Env = Map.Map String TypeScheme

findScheme :: String -> Env -> TypeScheme
findScheme n = fromJust . (Map.lookup n)

containsScheme :: String -> Env -> Bool
containsScheme = Map.member

addScheme :: String -> TypeScheme -> Env -> Env
addScheme = Map.insert

toScheme :: Type -> TypeScheme
toScheme = ForAll Set.empty

toEnv :: [(String, Type)] -> Env
toEnv = (Map.map toScheme) . Map.fromList
