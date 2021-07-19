module ConstraintsResolution where

import TypedAst (TypedExp)
import Ast (ExpF(..))
import Fixpoint (Fix(..))
import Annotations (Ann(..))
import Data.Set (fromList, toList)
import Types ( Pred(..), Type(TyCon, TyApp, TyVar), Qual(..), tyLam )
import TypesPrinter ()
import Data.Char ( toLower )

convertPreds :: TypedExp -> TypedExp
convertPreds (In (Ann (loc, ps :=> t) (Let n v b))) =
        In (Ann (loc, fromList [] :=> extendType (snd <$> args) t) (Let n v' b))
    where args = getNewArgs (toList ps)
          v' = foldl (\acc (n', t') -> lamWithType n' (fromList [] :=> t') acc) v args
convertPreds _ = undefined

extendType :: [Type] -> Type -> Type
extendType ts t = foldl (flip tyLam) t ts

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