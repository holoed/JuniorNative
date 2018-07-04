module InferMonad where

import Control.Monad
import Monads
import Types
import Substitutions
import Environment
import Data.Map (fromList)
import Data.Set (Set, toList, (\\), map, unions)
import Prelude hiding (map)

type TypeM = ReaderState (Env, Type, Set String) (Substitutions, Int)

newTyVar :: TypeM Type
newTyVar = do (subs, i) <- get
              put (subs, i + 1)
              return (TyVar ("T" ++ show i))

refreshNames :: Set String -> TypeM Substitutions
refreshNames ns = do (TyVar n') <- newTyVar
                     return $ fromList (fmap (\n -> (n, TyVar (n ++ n'))) (toList ns))

getBaseType :: TypeM Type
getBaseType = fmap (\(_,b,_) -> b) ask

getEnv :: TypeM Env
getEnv = fmap (\(env,_,_) -> env) ask

updateSubs :: (Substitutions -> TypeM Substitutions) -> TypeM ()
updateSubs f =
   do (subs, index) <- get
      subs' <- f subs
      put (subs', index)

-- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
mkForAll :: Set String -> Type -> TypeM Type
mkForAll sv t = do
  (subs, _) <- get
  let subSv = map (substitute subs . TyVar) sv
  let tyToRefresh = getTVarsOfType t \\ unions (toList (map getTVarsOfType subSv))
  fmap (`substitute` t) (refreshNames tyToRefresh)

getTypeForName :: String -> TypeM Type
getTypeForName n =
  do env <- getEnv
     unless (containsScheme n env) $ throwError ("Name " ++ n ++ " not found.")
     case findScheme n env of
       ForAll sv t -> mkForAll sv t
       Identity t -> return t

generalise :: Set String -> Type -> TypeScheme
generalise sv t@(TyLam _ _) = ForAll sv t
generalise _ t = Identity t
