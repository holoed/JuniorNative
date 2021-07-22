module MonomorphicRestriction where

import Data.Set (fromList, union)
import Data.Map (empty)
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypedAst ( TypedExp, TypedExpF )
import Ast ( ExpF(Let, Lam) )
import Types (Pred(..), Qual((:=>)), Type (TyVar))
import Substitutions (Substitutions, extend, substitute)
import BuiltIns (intCon, doubleCon)
import RecursionSchemes (cataRec)
import Control.Monad.State ( State, MonadState(get, put), execState ) 


defaultConstraint :: Pred -> Substitutions-> Substitutions
defaultConstraint (IsIn "Num" (TyVar n k)) = extend (n, k) intCon
defaultConstraint (IsIn "Fractional" (TyVar n k)) = extend (n, k) doubleCon
defaultConstraint  _ = id

applyRestriction :: TypedExp -> (TypedExp, Substitutions)
applyRestriction e@(In (Ann _ (Let _ (In (Ann _ (Lam _ _))) _))) = (e, empty)
applyRestriction e@(In (Ann (loc, ps1 :=> t) (Let n v@(In (Ann (_, ps2 :=> _) _)) b))) =
    (In (Ann (loc, fromList [] :=> substitute subs t) (Let n v b)), subs')
    where subs = foldr defaultConstraint empty (ps1 `union` ps2)
          subs' = execState (cataRec alg e) subs
          alg :: TypedExpF (State Substitutions TypedExp) -> State Substitutions TypedExp
          alg (Ann (l, qt@(ps3 :=> _)) x) = do
            subs'' <- get
            put $ foldr defaultConstraint subs'' ps3
            y <- sequenceA (Ann (l, qt) x)
            return $ In y
applyRestriction e = (e, empty)



    