module InferMonad where

import Control.Monad
import Control.Monad.Writer
import Monads
import Types
import Substitutions
import Environment
import Data.Map (fromList)
import Data.Set (Set, toList, (\\), map, unions)
import Prelude hiding (map)

type TypeM = ReaderWriterState (Env, Type, Set String) (Set Pred) (Substitutions, Int)

newTyVar :: TypeM Type
newTyVar = do (subs, i) <- get
              put (subs, i + 1)
              return (TyVar ("T" ++ show i) 0)

refreshNames :: Set String -> TypeM Substitutions
refreshNames ns = newTyVar >>= (\(TyVar n' k) -> return $ fromList (fmap (\n -> (n, TyVar (n ++ n') k)) (toList ns))) 

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
mkForAll :: Set String -> Qual Type -> TypeM (Qual Type)
mkForAll sv qt = do
  (subs, _) <- get
  let subSv = map (substitute subs . (\n -> TyVar n 0)) sv
  let tyToRefresh = getTVarsOfQType qt \\ unions (toList (map getTVarsOfType subSv))
  fmap (`substituteQ` qt) (refreshNames tyToRefresh)

getTypeForName :: String -> TypeM Type
getTypeForName n =
  do env <- getEnv
     unless (containsScheme n env) $ throwError ("Name " ++ n ++ " not found.")
     case findScheme n env of
       ForAll sv qt -> do (ps :=> t) <- mkForAll sv qt
                          tell ps
                          return t
       Identity (_ :=> t) -> return t

generalise :: Set String -> Qual Type -> TypeScheme
generalise sv (ps :=> t@(TyLam _ _)) = ForAll sv (ps :=> t)
generalise _ qt = Identity qt

substituteQM :: Qual Type -> TypeM (Qual Type)
substituteQM qt = do
  (subs, _) <- get
  return $ substituteQ subs qt