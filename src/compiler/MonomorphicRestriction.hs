module MonomorphicRestriction where

import Data.Set (toList)
import Data.Map (member)
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypedAst ( TypedExp, TypedExpF )
import Ast ( ExpF(Defn, Lam) )
import Types (Pred(..), Qual((:=>)), Type (TyVar))
import Substitutions (Substitutions, extend)
import BuiltIns (intCon, doubleCon)
import RecursionSchemes (cataRec)
import InferMonad (TypeM, substituteQM, updateSubs)

extendIfNotPresent :: (String, Int) -> Type -> Substitutions -> Substitutions
extendIfNotPresent k v subs =
    if member k subs then subs else extend k v subs

defaultConstraint :: Pred -> Substitutions-> Substitutions
defaultConstraint (IsIn "Num" (TyVar n k)) = extendIfNotPresent (n, k) intCon
defaultConstraint (IsIn "Integral" (TyVar n k)) = extendIfNotPresent (n, k) intCon
defaultConstraint (IsIn "Fractional" (TyVar n k)) = extendIfNotPresent (n, k) doubleCon
defaultConstraint (IsIn "Floating" (TyVar n k)) = extendIfNotPresent (n, k) doubleCon
defaultConstraint  _ = id

numClasses :: [String]
numClasses  = ["Num", "Integral", "Floating", "Fractional"]

stdClasses :: [String]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Functor", "Monad"] ++ numClasses

satisfy :: Qual Type -> Bool 
satisfy (ps :=> _) = 
    let is = [ i' | IsIn i' _ <- toList ps ] in
    any (`elem` numClasses) is  &&
    all (`elem` stdClasses) is 

applyRestriction :: TypedExp -> TypeM TypedExp
applyRestriction e@(In (Ann _ (Defn _ _ (In (Ann _ (Lam _ _)))))) = return e
applyRestriction e@(In (Ann (_, qt'@(ps :=> _)) (Defn Nothing _ _ ))) | satisfy qt' =
    do  updateSubs (\subs -> return $ foldr defaultConstraint subs ps)
        cataRec alg e
    where alg :: TypedExpF (TypeM TypedExp) -> TypeM TypedExp
          alg (Ann (l, qt) x) = do
            qt'' <- substituteQM qt  
            y <- sequenceA (Ann (l, qt'') x)
            return $ In y
applyRestriction e = return e



    