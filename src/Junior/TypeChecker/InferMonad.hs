module Junior.TypeChecker.InferMonad where

import Junior.Parser.Location (Loc, PString(..))
import Control.Monad ( unless )
import Control.Monad.Writer( MonadWriter(tell) )
import Junior.TypeChecker.Monads ( ErrorReaderWriterState, ask, get, put, throwError )
import Junior.Core.Types ( TypeScheme(..), Type(..), Qual(..), Pred, getTVarsOfType, getTVarsOfQType )
import Junior.TypeChecker.Substitutions ( Substitutions, substitute, substituteQ )
import Junior.TypeChecker.Environment ( Env, findScheme, containsScheme )
import Data.Map (fromList)
import Data.Set (Set, toList, (\\), map, unions)
import Prelude hiding (map)

type TypeM = ErrorReaderWriterState PString (Env, Type, Set String, Bool) (Set Pred) (Substitutions, Int)

newTyVar :: Int -> TypeM Type
newTyVar k = do (subs, i) <- get
                put (subs, i + 1)
                return (TyVar ("T" ++ show i) k)

refreshNames :: Set (String, Int) -> TypeM Substitutions
refreshNames ns = newTyVar 0 >>= (\(TyVar n' k) -> return $ fromList (fmap (\(n, k') -> ((n, k'), TyVar (n ++ n') k')) (toList ns)))

getBaseType :: TypeM Type
getBaseType = fmap (\(_,b,_,_) -> b) ask

getEnv :: TypeM Env
getEnv = fmap (\(env,_,_,_) -> env) ask

updateSubs :: (Substitutions -> TypeM Substitutions) -> TypeM ()
updateSubs f =
   do (subs, index) <- get
      subs' <- f subs
      put (subs', index)

-- https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
mkForAll :: Set String -> Qual Type -> TypeM (Qual Type)
mkForAll sv qt = do
  (subs, _) <- get
  let subSv = map (substitute subs . (`TyVar` 0)) sv
  let tyToRefresh = getTVarsOfQType qt \\ unions (toList (map getTVarsOfType subSv))
  fmap (`substituteQ` qt) (refreshNames tyToRefresh)

getTypeForName :: Loc -> String -> TypeM Type
getTypeForName l n =
  do env <- getEnv
     unless (containsScheme n env) $ throwError $ PStr ("Name " ++ n ++ " not found.", Just l)
     case findScheme n env of
       ForAll sv qt -> do (ps :=> t) <- mkForAll sv qt
                          tell ps
                          return t
       Identity (_ :=> t) -> return t

generalise :: Bool -> Set String -> Qual Type -> TypeScheme
generalise True sv (ps :=> t@(TyApp (TyApp (TyCon "->") _) _)) = ForAll sv (ps :=> t)
generalise _ _ qt = Identity qt

substituteQM :: Qual Type -> TypeM (Qual Type)
substituteQM qt = do
  (subs, _) <- get
  return $ substituteQ subs qt