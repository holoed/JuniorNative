module ConstraintsResolution where

import TypedAst (TypedExp, tapp, tvar)
import Ast (ExpF(..), extractNameFromPat)
import Fixpoint (Fix(..))
import Annotations (Ann(..), mapAnn)
import qualified Data.Set as Set (Set, fromList, toList, union, empty, member)
import qualified Data.Map as Map
import Types ( Pred(..), Type(TyCon, TyApp, TyVar), Qual(..), tyLam, isLam, TypeScheme(..), getTVarsOfPred )
import TypesPrinter ()
import Data.Char ( toLower )
import RecursionSchemes (cataRec)
import Location (Loc)
import Environment (Env, containsScheme, findScheme)
import MonomorphicRestriction ( defaultConstraint )
import Substitutions ( substitutePredicate )

filterOutTautologies :: [Pred] -> [Pred]
filterOutTautologies = filter f
    where f p = getTVarsOfPred p /= Set.empty

getTopLevelPreds :: String -> Env -> [Pred]
getTopLevelPreds n env =
    if containsScheme n env then
    case findScheme n env of
        (ForAll _ (ps :=> t)) -> Set.toList ps
        (Identity (ps :=> t)) -> Set.toList ps
    else []


convertPreds :: Env -> TypedExp -> TypedExp
convertPreds env (In (Ann (loc, ps :=> t) (Let n v b))) =
        In (Ann (loc, Set.fromList [] :=> extendType (typeForPred <$> Set.toList ps) t) (Let n v'' b))
    where (_, name) = extractNameFromPat (mapAnn fst n)
          ps' = if isLam t then collectPreds v else Set.empty
          args = getNewArgs ((filterOutTautologies . Set.toList) ps')
          v'  = convertBody env name ps' args v
          v'' = foldr (\(n', t') acc -> lamWithType n' (Set.fromList [] :=> t') acc) v' args
convertPreds _ _ = undefined

collectPreds :: TypedExp -> Set.Set Pred
collectPreds (In (Ann _ (Lam (In (Ann (_, ps :=> _) (VarPat _))) v))) =  ps `Set.union` collectPreds v
collectPreds (In (Ann (_, ps :=> _) _)) = ps

defaultIfNotInScope :: Set.Set Pred -> [Pred] -> [Pred]
defaultIfNotInScope parent_ps ps = do
    p <- ps
    return $ if Set.member p parent_ps then p
    else substitutePredicate (defaultConstraint p Map.empty) p

convertBody :: Env -> String -> Set.Set Pred -> [(String, Type)] -> TypedExp -> TypedExp
convertBody env name parent_ps parent_args = cataRec alg
       where alg e@(Ann (Just loc, ps :=> _) (Var n)) =
                let args = if n == name
                    then parent_args
                    else getNewArgs (
                        let re = defaultIfNotInScope parent_ps (Set.toList ps) in
                        if null re then defaultIfNotInScope parent_ps (getTopLevelPreds n env)
                        else re
                     ) in
                applyArgs loc args (In e)
             alg x = In x

applyArgs :: Loc -> [(String, Type)] -> TypedExp -> TypedExp
applyArgs loc args e =
    foldl (\acc (n, t) ->
        let qt = Set.fromList [] :=> t in
        tapp qt acc (tvar loc qt n)) e args

extendType :: [Type] -> Type -> Type
extendType ts t = foldr tyLam t ts

lamWithType :: String -> Qual Type -> TypedExp -> TypedExp
lamWithType n qt1 e@(In (Ann (loc, qt2) _)) =
    In (Ann (loc, qt2) (Lam (In (Ann (loc, qt1) (VarPat n))) e))

getNewArgs :: [Pred] -> [(String, Type)]
getNewArgs ps = do p <- ps
                   return (varNameForPred p, typeForPred p)

toCamel :: String -> String
toCamel "" = ""
toCamel (x:xs) = toLower x : xs

typeForPred :: Pred -> Type
typeForPred (IsIn name t) = TyApp (TyCon name) t

varNameForPred :: Pred -> String
varNameForPred = toCamel . f
    where f :: Pred -> String
          f (IsIn name (TyVar n k)) = name ++ n ++ show k
          f (IsIn name (TyCon n)) = name ++ n
          f (IsIn name t) = name ++ show t