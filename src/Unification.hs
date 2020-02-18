module Unification where

import Data.Set (member)
import Control.Monad (foldM_)
import Types
import Substitutions
import Monads
import InferMonad

mgu :: Type -> Type -> TypeM ()
mgu a b =
  do
    (subs, _) <- get
    case (substitute subs a, substitute subs b) of
      (TyVar ta _, TyVar tb _) | ta == tb -> return ()
      (TyVar ta _, _) | not (member ta (getTVarsOfType b)) -> updateSubs (return . extend ta b)
      (_, TyVar _ _) -> mgu b a
      (TyLam a1 b1, TyLam a2 b2) -> do mgu b1 b2
                                       mgu a1 a2
      (TyApp a1 b1, TyApp a2 b2) -> do mgu a1 a2
                                       mgu b1 b2                                       
      (TyCon name1, TyCon name2) | name1 == name2 -> return ()
      (x, y) -> throwError ("Unable to unify " ++ show x ++ " with " ++ show y)

mguPred :: Pred -> Pred -> TypeM ()
mguPred (IsIn n1 t1) (IsIn n2 t2) | n1 == n2 = mgu t1 t2
mguPred _ _ = fail "Classes Differ"
