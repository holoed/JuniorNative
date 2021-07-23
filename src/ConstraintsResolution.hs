{-# LANGUAGE TupleSections #-}
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
import Substitutions ( substitute, substitutePredicate, mappings )
import Data.Either (fromRight)
import ContextReduction ( ClassEnv, classes )

filterOutTautologies :: [Pred] -> [Pred]
filterOutTautologies = filter f
    where f p = getTVarsOfPred p /= Set.empty

getTypeForName :: String -> Env -> Qual Type
getTypeForName n env =
    case findScheme n env of
        (ForAll _ qt) -> qt
        (Identity qt) -> qt

convertPreds :: ClassEnv -> Env -> TypedExp -> TypedExp
convertPreds classEnv env (In (Ann (loc, ps :=> t) (Let n v b))) =
        In (Ann (loc, Set.fromList [] :=> extendType (typeForPred <$> Set.toList ps) t) (Let n v'' b))
    where (_, name) = extractNameFromPat (mapAnn fst n)
          ps' = if isLam t then collectPreds v else Set.empty
          args = getNewArgs ((filterOutTautologies . Set.toList) ps')
          v'  = convertBody classEnv env name args v
          v'' = foldr (\(n', t') acc -> lamWithType n' (Set.fromList [] :=> t') acc) v' args
convertPreds _ _ _ = undefined

collectPreds :: TypedExp -> Set.Set Pred
collectPreds (In (Ann _ (Lam (In (Ann (_, ps :=> _) (VarPat _))) v))) =  ps `Set.union` collectPreds v
collectPreds (In (Ann (_, ps :=> _) _)) = ps

defaultIfNotInScope :: Set.Set Pred -> [Pred] -> [Pred]
defaultIfNotInScope parent_ps ps = do
    p <- ps
    return $ if Set.member p parent_ps then p
    else substitutePredicate (defaultConstraint p Map.empty) p

mapEnvWithLocal :: Env -> String -> Qual Type -> ([Pred], Type)
mapEnvWithLocal env name (_ :=> t1) =
    if containsScheme name env then
        let (ps2 :=> t2) = getTypeForName name env in
        let subs = fromRight (error "Mapping failed") (mappings t2 t1) in
        let resolvedPredicates = substitutePredicate subs <$> Set.toList ps2 in
        let resolvedType = substitute subs t2 in
        (resolvedPredicates, resolvedType)
    else ([], t1)

buildClassHiearchy :: ClassEnv -> (String, Type) -> Map.Map (String, Type) (String, Type)
buildClassHiearchy classEnv v@(_, TyApp (TyCon className) t') =
    let classDict = classes classEnv in
    let (parents, _) = (Map.!) classDict className in
    let args = getNewArgs ((`IsIn` t') <$> parents) in
    Map.fromList ((, v) <$> args)
buildClassHiearchy _ _ = undefined

buildHierchyForAllArgs :: ClassEnv -> [(String, Type)] -> Map.Map (String, Type) (String, Type)
buildHierchyForAllArgs classEnv = foldl (\acc x -> acc `Map.union` buildClassHiearchy classEnv x) Map.empty

mapCompatibleTypes :: ClassEnv -> [(String, Type)] -> [(String, Type)] -> [(String, Type)]
mapCompatibleTypes classEnv parent_args child_args =
   (\arg -> Map.findWithDefault arg arg dict)  <$> child_args
   where dict = buildHierchyForAllArgs classEnv parent_args

convertBody :: ClassEnv -> Env -> String -> [(String, Type)] -> TypedExp -> TypedExp
convertBody classEnv env name parent_args = cataRec alg
       where alg e@(Ann (Just loc, qt) (Var n)) =
                let (psList, _) = mapEnvWithLocal env n qt in
                let args = if n == name
                    then parent_args
                    else mapCompatibleTypes classEnv parent_args (getNewArgs psList) in
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