module ParserUtils where

import Types (Qual ((:=>)), Type(TyCon, TyVar, TyApp))
import Data.Set (fromList)
import PAst (SynExp, SynExpF (Var, InfixApp))
import Debug.Trace (trace)
import RecursionSchemes (cataRec)
import Annotations (Ann(Ann))
import Data.Char (isLower)
import Operators (juxtaOp)

fromExprToQualType :: SynExp -> Qual Type
fromExprToQualType e = fromList [] :=> cataRec alg e
    where
     alg :: Ann a SynExpF Type -> Type
     alg (Ann _ (Var n)) | isLower (head n) = TyVar n 0
     alg (Ann _ (Var n)) = TyCon n
     alg (Ann _ (InfixApp op e1 e2)) | op == juxtaOp = TyApp e1 e2