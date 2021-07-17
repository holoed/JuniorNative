module ConstraintsResolution where

import TypedAst (TypedExp)
import Ast (Exp)
import Annotations (mapAnn)
import Types ( Pred(..), Type(TyCon, TyApp, TyVar) )
import TypesPrinter () 
import Data.Char ( toLower )

process :: TypedExp -> Exp
process = mapAnn fst

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