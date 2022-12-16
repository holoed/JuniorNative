module Junior.TypeChecker.Environment where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Junior.Core.Types ( TypeScheme(ForAll, Identity), Type, Qual )
import Prelude
import Data.Bifunctor (second)

type Env = Map.Map String TypeScheme

findScheme :: String -> Env -> TypeScheme
findScheme n env = 
    case Map.lookup n env of 
        Just x -> x
        Nothing -> error ("Can't find " ++ n)

containsScheme :: String -> Env -> Bool
containsScheme = Map.member

addScheme :: String -> TypeScheme -> Env -> Env
addScheme = Map.insert

toScheme :: Qual Type -> TypeScheme
toScheme = ForAll Set.empty

fromScheme :: TypeScheme -> Qual Type
fromScheme (ForAll _ qt) = qt
fromScheme (Identity qt) = qt

toEnv :: [(String, Qual Type)] -> Env
toEnv = Map.map toScheme . Map.fromList

fromEnv :: Env -> [(String, Qual Type)]
fromEnv = fmap (second fromScheme) . Map.toList

concatEnvs :: Env -> Env -> Env
concatEnvs x y = x `Map.union` y