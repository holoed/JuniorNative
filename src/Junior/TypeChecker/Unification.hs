module Junior.TypeChecker.Unification where

import Junior.Parser.Location (Loc, PString (PStr))
import Junior.Core.Ast ()
import Data.Set (member)
import Junior.Core.Types ( Type(..), Pred(..), getTVarsOfType )
import Junior.Pretty.TypesPrinter ()
import Junior.TypeChecker.Substitutions ( extend, substitute )
import Junior.TypeChecker.Monads ( get, throwError )
import Junior.TypeChecker.InferMonad ( TypeM, updateSubs )

mgu :: Loc -> Type -> Type -> TypeM ()
mgu l a b =
  do
    (subs, _) <- get
    case (substitute subs a, substitute subs b) of
      (TyVar ta _, TyVar tb _) | ta == tb -> return ()
      (TyVar ta k, b') | not (member (ta, k) (getTVarsOfType b')) -> updateSubs (return . extend (ta, k) b')
      (_, TyVar _ _) -> mgu l b a
      (TyApp a1 b1, TyApp a2 b2) -> do mgu l a1 a2
                                       mgu l b1 b2                                       
      (TyCon name1, TyCon name2) | name1 == name2 -> return ()
      (x, y) -> throwError $ PStr ("Unable to unify " ++ show x ++ " with " ++ show y, Just l)

mguPred :: Loc -> Pred -> Pred -> TypeM ()
mguPred l (IsIn n1 t1) (IsIn n2 t2) | n1 == n2 = mgu l t1 t2
mguPred l _ _ = throwError $ PStr ("Classes Differ", Just l)
