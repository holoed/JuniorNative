module ConstraintsResolution where

import TypedAst (TypedExp, tapp, tvar)
import Ast (ExpF(..), extractNameFromPat)
import Fixpoint (Fix(..))
import Annotations (Ann(..), mapAnn)
import qualified Data.Set as Set (Set, fromList, toList, union, empty)
import Types ( Pred(..), Type(TyCon, TyApp, TyVar), Qual(..), tyLam, TypeScheme(..), getTVarsOfPred )
import TypesPrinter ()
import Data.Char ( toLower )
import RecursionSchemes (cataRec)
import Location (Loc)
import Substitutions ( Substitutions, substitutePredicate )

filterOutTautologies :: [Pred] -> [Pred]
filterOutTautologies = filter f
    where f p = getTVarsOfPred p /= Set.empty

convertPreds :: (TypedExp, Substitutions) -> TypedExp
convertPreds (In (Ann (loc, ps :=> t) (Let n v b)), subs) =
        In (Ann (loc, Set.fromList [] :=> extendType (typeForPred <$> Set.toList ps) t) (Let n v'' b))
    where ps' = collectPreds v
          (_, name) = extractNameFromPat (mapAnn fst n)
          args = getNewArgs ((filterOutTautologies . (substitutePredicate subs <$>) . Set.toList) ps')
          v'  = convertBody name args subs v
          v'' = foldr (\(n', t') acc -> lamWithType n' (Set.fromList [] :=> t') acc) v' args
convertPreds _ = undefined

collectPreds :: TypedExp -> Set.Set Pred
collectPreds (In (Ann _ (Lam (In (Ann (_, ps :=> _) (VarPat _))) v))) =  ps `Set.union` collectPreds v
collectPreds (In (Ann (_, ps :=> _) _)) = ps

fromTypeSchemeToPreds :: TypeScheme -> Set.Set Pred
fromTypeSchemeToPreds (ForAll _ (ps :=> _)) = ps
fromTypeSchemeToPreds (Identity (ps :=> _)) = ps

convertBody :: String -> [(String, Type)] -> Substitutions -> TypedExp -> TypedExp
convertBody name parent_args subs = cataRec alg
       where alg e@(Ann (Just loc, ps :=> _) (Var n)) =
                let args = if n == name
                    then parent_args
                    else getNewArgs (((substitutePredicate subs <$>) . Set.toList) ps) in
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