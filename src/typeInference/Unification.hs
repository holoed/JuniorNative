module TypeInference.Unification where

import Data.Set (member)
import Control.Monad (foldM)
import TypeInference.Types
import TypeInference.Substitutions
import Utilities.Monads
import TypeInference.InferMonad

mgu :: Type -> Type -> Substitutions -> TypeM Substitutions
mgu a b subs =
  case (substitute subs a, substitute subs b) of
    (TyVar ta, TyVar tb) | ta == tb -> return subs
    (TyVar ta, _) | not (member ta (getTVarsOfType b)) -> return (extend ta b subs)
    (_, TyVar _) -> mgu b a subs
    (TyLam a1 b1, TyLam a2 b2) -> do subs2 <- mgu b1 b2 subs
                                     mgu a1 a2 subs2
    (TyCon name1 args1, TyCon name2 args2) | name1 == name2 ->
                         foldM (\s (a', b') -> mgu a' b' s) subs (zip args1 args2)
    (x, y) -> throwError ("Unable to unify " ++ show x ++ " with " ++ show y)
