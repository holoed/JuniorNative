module MonomorphicRestriction where

import Data.Map (empty)
import Data.Set (Set)
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypedAst ( TypedExp )
import Ast ( ExpF(Let, Lam) )
import Types (Pred(..), Qual((:=>)), Type (TyVar), deleteTautology, clean)
import Substitutions (Substitutions, extend, substituteQ)
import BuiltIns (intCon, doubleCon)
import RecursionSchemes (cataRec)

bar :: Pred -> Substitutions-> Substitutions
bar (IsIn "Num" (TyVar n k)) = extend (n, k) intCon
bar (IsIn "Fractional" (TyVar n k)) = extend (n, k) doubleCon
bar  _ = id

foo :: Set Pred -> Substitutions
foo = foldr bar empty

process :: TypedExp -> TypedExp
process e@(In (Ann _ (Let _ (In (Ann _ (Lam _ _))) _))) = e
process e@(In (Ann (_, ps :=> _) Let {})) =
    cataRec alg e
    where subs = foo ps
          alg (Ann (loc, qt) e'@Let{}) = In (Ann (loc, (deleteTautology . clean . substituteQ subs) qt) e')
          alg (Ann (loc, qt) e') = In (Ann (loc, substituteQ subs qt) e')
process e = e

       