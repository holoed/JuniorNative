{-# LANGUAGE TupleSections #-}
module ConstraintsResolution where

import TypedAst (TypedExp, tapp, tvar)
import Ast (ExpF(..), extractNameFromPat)
import Fixpoint (Fix(..))
import Annotations (Ann(..), mapAnn)
import qualified Data.Set as Set (Set, fromList, toList, union)
import qualified Data.Map as Map
import Types ( Pred(..), Type(TyCon, TyApp), Qual(..), tyLam, isLam, TypeScheme(..) )
import TypesPrinter ()
import Data.Char ( toLower )
import RecursionSchemes (cataRec)
import Location (zeroLoc)
import Environment (Env, containsScheme, findScheme)
import Substitutions ( substitute, substitutePredicate, mappings )
import Data.Either (fromRight)
import ContextReduction ( ClassEnv, classes )
import Data.Maybe (isJust, isNothing, listToMaybe, fromJust)
import BuiltIns (tupleCon, untuple)
import Data.List ( find )

getTypeForName :: String -> Env -> Qual Type
getTypeForName n env =
    case findScheme n env of
        (ForAll _ qt) -> qt
        (Identity qt) -> qt

convertPreds :: ClassEnv -> Env -> TypedExp -> TypedExp
convertPreds classEnv env (In (Ann (loc, ps :=> t) (Let n v b))) =
        In (Ann (loc, Set.fromList [] :=> extendType (typeForPred <$> Set.toList ps) t) (Let n v'' b))
    where (_, name) = extractNameFromPat (mapAnn fst n)
          ps' = if isLam t then Set.toList (collectPreds v) else []
          args = getArgs ps'
          v'  = convertBody classEnv env name args v
          v'' = foldr (\(In (Ann (_, qt) (Var n'))) acc -> lamWithType n' qt acc) v' args
convertPreds _ _ _ = undefined

collectPreds :: TypedExp -> Set.Set Pred
collectPreds (In (Ann _ (Lam (In (Ann (_, ps :=> _) (VarPat _))) v))) =  ps `Set.union` collectPreds v
collectPreds (In (Ann (_, ps :=> _) _)) = ps

mapEnvWithLocal :: Env -> String -> Qual Type -> ([Pred], Type)
mapEnvWithLocal env name (_ :=> t1) =
    if containsScheme name env then
        let (ps2 :=> t2) = getTypeForName name env in
        let subs = fromRight (error "Mapping failed") (mappings t2 t1) in
        let resolvedPredicates = substitutePredicate subs <$> Set.toList ps2 in
        let resolvedType = substitute subs t2 in
        (resolvedPredicates, resolvedType)
    else ([], t1)

buildClassHiearchy :: ClassEnv -> TypedExp -> Map.Map String TypedExp
buildClassHiearchy classEnv v@(In (Ann (_, _  :=> TyApp (TyCon className) t') _)) =
    let classDict = classes classEnv in
    let (parents, _) = (Map.!) classDict className in
    let args = getArgs ((`IsIn` t') <$> parents) in
    Map.fromList ((\(In (Ann _ (Var k))) -> (k, v)) <$> args)
buildClassHiearchy _ _ =  undefined

buildHierchyForAllArgs :: ClassEnv -> [TypedExp] -> Map.Map String TypedExp
buildHierchyForAllArgs classEnv = foldl (\acc x -> acc `Map.union` buildClassHiearchy classEnv x) Map.empty

parentContains :: String -> [TypedExp] -> Bool
parentContains n  = 
    isJust . find (\(In (Ann _ (Var k))) -> k == n)

mapCompatibleTypes :: ClassEnv -> [TypedExp] -> [TypedExp] -> [TypedExp] -> [TypedExp]
mapCompatibleTypes classEnv parent_args child_args resolved_args =
   (\(arg@(In (Ann _ (Var k2))), arg3) ->
       if parentContains k2 parent_args then arg
       else Map.findWithDefault arg3 k2 dict)  <$> zip child_args resolved_args
   where dict = buildHierchyForAllArgs classEnv parent_args

convertBody :: ClassEnv -> Env -> String -> [TypedExp] -> TypedExp -> TypedExp
convertBody classEnv env name parent_args = cataRec alg
       where alg e@(Ann (_, qt) (Var n)) =
                let (psList, _) = mapEnvWithLocal env n qt in
                let args = if n == name
                    then parent_args
                    else mapCompatibleTypes classEnv parent_args (getArgs psList) (getNewArgs classEnv psList) in
                applyArgs args (In e)
             alg x = In x

applyArgs :: [TypedExp] -> TypedExp -> TypedExp
applyArgs args e =
    foldl (\acc@(In (Ann (_, qt) _)) arg -> tapp qt acc arg) e args

extendType :: [Type] -> Type -> Type
extendType ts t = foldr tyLam t ts

lamWithType :: String -> Qual Type -> TypedExp -> TypedExp
lamWithType n qt1 e@(In (Ann (loc, qt2) _)) =
    In (Ann (loc, qt2) (Lam (In (Ann (loc, qt1) (VarPat n))) e))

compatibleType :: Pred -> Qual Pred -> [([Pred], Pred)]
compatibleType p (ps :=> p') =
    case mappings (typeForPred p') (typeForPred p) of
        Left _ -> []
        Right subs ->
            let p'' = substitutePredicate subs p in
            let ps' = substitutePredicate subs <$> Set.toList ps in
            [(ps',  p'')]

findInstance :: ClassEnv -> Pred -> Maybe ([Pred], Pred)
findInstance classEnv p@(IsIn name _) =
    listToMaybe (instances >>= compatibleType p) 
    where dict = classes classEnv
          (_, instances) = (Map.!) dict name

createExpFromType :: Type -> TypedExp
createExpFromType ty@(TyApp (TyApp (TyCon "->") tuple) (TyApp (TyCon n) _))  = 
    let ts = untuple tuple in
    applyArgs (createExpFromType <$> ts) (tvar zeroLoc (Set.fromList [] :=> ty) (toCamel (n ++ "Tuple" ++ show (length ts))))
createExpFromType t@(TyApp (TyCon n) t') = tvar zeroLoc (Set.fromList [] :=> t) (toCamel (n ++ show t'))
createExpFromType _ = undefined 

getArgs :: [Pred] -> [TypedExp]
getArgs ps =
    do p <- ps
       return $ tvar zeroLoc (Set.fromList [] :=> typeForPred p) (varNameForPred p)

getNewArgs :: ClassEnv -> [Pred] -> [TypedExp]
getNewArgs classEnv ps =
    do p <- ps
       let inst = findInstance classEnv p
       if isNothing inst then
           return $ tvar zeroLoc (Set.fromList [] :=> typeForPred p) (varNameForPred p)
       else let (ps', p') = fromJust inst in
            let ts = typeForPred <$> ps' in
            let t = typeForPred p' in
            return $ createExpFromType (buildType ts t)

toCamel :: String -> String
toCamel "" = ""
toCamel (x:xs) = toLower x : xs

typeForPred :: Pred -> Type
typeForPred (IsIn name t) = TyApp (TyCon name) t

buildType :: [Type] -> Type -> Type
buildType [] t = t
buildType [t'] t = tyLam t' t
buildType ts t = tyLam (tupleCon ts) t

varNameForPred :: Pred -> String
varNameForPred = toCamel . f
    where f :: Pred -> String
          f (IsIn name t) = name ++ show t