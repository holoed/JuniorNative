{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Junior.TypeChecker.ConstraintsResolution where

import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF, tapp, tvar)
import Junior.Core.Ast (ExpF(..), extractNameFromPat)
import Junior.Utils.Fixpoint (Fix(..))
import Junior.Utils.Annotations (Ann(..), mapAnn)
import qualified Data.Set as Set (Set, fromList, toList, union)
import qualified Data.Map as Map
import Junior.Core.Types ( Pred(..), Type(TyCon, TyApp), Qual(..), tyLam, isLam, TypeScheme(..) )
import Junior.Pretty.TypesPrinter ()
import Data.Char ( toLower )
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Parser.Location (zeroLoc)
import Junior.TypeChecker.Environment (Env, containsScheme, findScheme)
import Junior.TypeChecker.Substitutions ( substitute, substitutePredicate, mappings )
import Junior.TypeChecker.ContextReduction ( ClassEnv, classes, inHnf )
import Data.Maybe (isJust, isNothing, listToMaybe, fromJust)
import Junior.Core.BuiltIns (tupleCon)
import Data.List ( find )
import Control.Monad.Reader (Reader, runReader, ask, local)

getTypeForName :: String -> Env -> Qual Type
getTypeForName n env =
    case findScheme n env of
        (ForAll _ qt) -> qt
        (Identity qt) -> qt

convertPreds :: ClassEnv -> Env -> TypedExp -> TypedExp
convertPreds classEnv env (In (Ann (loc, ps :=> t) (Defn givenQt n v))) =
        In (Ann (loc, Set.fromList [] :=> extendType (typeForPred <$> Set.toList ps) t) (Defn givenQt n v''))
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
        let subs = either (\x -> error ("Mapping failed " ++ show x)) id (mappings t2 t1) in
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

extractNames :: TypedExp -> [String]
extractNames (In (Ann _ (VarPat s))) = [s]
extractNames (In (Ann _ (TuplePat xs))) = xs >>= extractNames
extractNames (In (Ann _ (ConPat _ xs))) = xs >>= extractNames
extractNames _ = error "Unsupported"

convertBody :: ClassEnv -> Env -> String -> [TypedExp] -> TypedExp -> TypedExp
convertBody classEnv baseEnv name parent_args = flip runReader baseEnv . cataRec alg
       where alg :: TypedExpF (Reader Env TypedExp) -> Reader Env TypedExp
             alg (Ann (l, qt) (Lam s e)) = do
                 s'  <- s
                 let ns = extractNames s'
                 e' <- local (\env2 -> foldr Map.delete env2 ns) e
                 return $ In (Ann (l, qt) (Lam s' e'))
             alg (Ann (l, qt) (Var n)) = do
                env <- ask
                let (psList, _) = mapEnvWithLocal env n qt
                let args = if n == name
                    then parent_args
                    else mapCompatibleTypes classEnv parent_args (getArgs psList) (getNewArgs classEnv psList)
                return $ applyArgs args (In (Ann (l, qt) (Var n)))
             alg x = fmap In (sequence x)

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
createExpFromType ty@(TyApp (TyApp (TyCon "->") (TyApp (TyApp (TyCon "Tuple") t1) t2)) (TyApp (TyCon n) _))  =
    applyArgs (createExpFromType <$> [t1, t2]) (tvar zeroLoc (Set.fromList [] :=> ty) (toCamel (n ++ "Tuple2")))
createExpFromType ty@(TyApp (TyApp (TyCon "->") t1) (TyApp (TyCon n) (TyApp t2 _)))  =
    applyArgs (createExpFromType <$> [t1]) (tvar zeroLoc (Set.fromList [] :=> ty) (toCamel (n ++ show t2)))
createExpFromType t@(TyApp (TyCon n) (TyApp t1 _)) = tvar zeroLoc (Set.fromList [] :=> t) (toCamel (filter (/= ' ') (n ++ show t1)))
createExpFromType t@(TyApp (TyCon n) t') = tvar zeroLoc (Set.fromList [] :=> t) (toCamel (filter (/= ' ') (n ++ show t')))
createExpFromType _ = undefined

getArgs :: [Pred] -> [TypedExp]
getArgs ps =
    do p <- ps
       return $ tvar zeroLoc (Set.fromList [] :=> typeForPred p) (varNameForPred p)

resolvePredsToType :: ClassEnv -> [Pred] -> [Type]
resolvePredsToType classEnv ps =
    do p <- ps
       let inst = findInstance classEnv p
       if isNothing inst then
           return $ buildType [] (typeForPred p)
       else let (ps', p') = fromJust inst in
            let ts = ps' >>= (\p2 -> if inHnf p2 then [typeForPred p2]
                                     else resolvePredsToType classEnv [p2]) in
            let t = typeForPred p' in
            return $ buildType ts t

getNewArgs :: ClassEnv -> [Pred] -> [TypedExp]
getNewArgs classEnv ps = createExpFromType <$> resolvePredsToType classEnv ps

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
          f (IsIn name t) = filter (/= ' ') (name ++ show t)