module MonomorphicRestriction where

import Data.Map (empty, member)
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypedAst ( TypedExp, TypedExpF )
import Ast ( ExpF(Defn, Lam) )
import Types (Pred(..), Qual((:=>)), Type (TyVar))
import Substitutions (Substitutions, extend, substituteQ)
import BuiltIns (intCon, doubleCon)
import RecursionSchemes (cataRec)
import Control.Monad.State ( State, MonadState(get, put), evalState )
import InferMonad (TypeM)

extendIfNotPresent :: (String, Int) -> Type -> Substitutions -> Substitutions
extendIfNotPresent k v subs =
    if member k subs then subs else extend k v subs

defaultConstraint :: Pred -> Substitutions-> Substitutions
defaultConstraint (IsIn "Eq" (TyVar n k)) = extendIfNotPresent (n, k) intCon
defaultConstraint (IsIn "Num" (TyVar n k)) = extendIfNotPresent (n, k) intCon
defaultConstraint (IsIn "Fractional" (TyVar n k)) = extendIfNotPresent (n, k) doubleCon
defaultConstraint  _ = id

applyRestriction :: TypedExp -> TypeM TypedExp
applyRestriction e@(In (Ann _ (Defn _ (In (Ann _ (Lam _ _)))))) = return e
applyRestriction e@(In (Ann _ Defn {})) =
    return $ evalState (cataRec alg e) empty
    where alg :: TypedExpF (State Substitutions TypedExp) -> State Substitutions TypedExp
          alg (Ann (l, qt@(ps3 :=> _)) x) = do
            subs'' <- get
            let subs''' = foldr defaultConstraint subs'' ps3
            put subs'''
            y <- sequenceA (Ann (l, substituteQ subs''' qt) x)
            return $ In y
applyRestriction e = return e



    