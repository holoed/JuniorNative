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
      (TyVar ta, TyVar tb) | ta == tb -> return ()
      (TyVar ta, _) | not (member ta (getTVarsOfType b)) -> updateSubs (return . extend ta b)
      (_, TyVar _) -> mgu b a
      (TyLam a1 b1, TyLam a2 b2) -> do mgu b1 b2
                                       mgu a1 a2
      (TyCon name1 args1, TyCon name2 args2) | name1 == name2 && length args1 == length args2 ->
                           foldM_ (\_ (a', b') -> mgu a' b') () (zip args1 args2)
      (x, y) -> throwError ("Unable to unify " ++ show x ++ " with " ++ show y)

mguPred :: Pred -> Pred -> TypeM ()
mguPred (IsIn n1 t1) (IsIn n2 t2) | n1 == n2 = mgu t1 t2
mguPred _ _ = fail "Classes Differ"
