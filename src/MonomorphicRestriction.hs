module MonomorphicRestriction where

import Data.Map (empty)
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypedAst ( TypedExp )
import Ast ( ExpF(Let, Lam) )
import Types (Pred(..), Qual((:=>)), Type (TyVar), deleteTautology, clean)
import Substitutions (Substitutions, extend, substituteQ)
import BuiltIns (intCon, doubleCon)
import RecursionSchemes (cataRec)

defaultConstraint :: Pred -> Substitutions-> Substitutions
defaultConstraint (IsIn "Num" (TyVar n k)) = extend (n, k) intCon
defaultConstraint (IsIn "Fractional" (TyVar n k)) = extend (n, k) doubleCon
defaultConstraint  _ = id

applyRestriction :: TypedExp -> TypedExp
applyRestriction e@(In (Ann _ (Let _ (In (Ann _ (Lam _ _))) _))) = e
applyRestriction e@(In (Ann (_, ps :=> _) Let {})) =
    cataRec alg e
    where subs = foldr defaultConstraint empty ps
          alg (Ann (loc, qt) e'@Let{}) = In (Ann (loc, (deleteTautology . clean . substituteQ subs) qt) e')
          alg (Ann (loc, qt) e') = In (Ann (loc, substituteQ subs qt) e')
applyRestriction e = e

       