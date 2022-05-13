module ParserUtils where

import Types (Qual ((:=>)), Type(TyCon, TyVar, TyApp), Pred (IsIn), tyLam)
import Data.Set (Set, fromList)
import PAst (SynExp, SynExpF (Var, InfixApp, MkTuple))
import RecursionSchemes (cataRec)
import Annotations (Ann(Ann))
import Data.Char (isLower)
import Operators (juxtaOp, classOp, lamOp)
import Fixpoint (Fix(In))
import BuiltIns (tupleCon)
import Location (Loc)

fromExprToType :: SynExp -> Type
fromExprToType = cataRec alg 
    where
     alg :: Ann (Maybe Loc) SynExpF Type -> Type
     alg (Ann _ (Var n)) | isLower (head n) = TyVar n 0
     alg (Ann _ (Var n)) = TyCon n
     alg (Ann _ (InfixApp op e1 e2)) | op == juxtaOp = TyApp e1 e2
     alg (Ann _ (InfixApp op e1 e2)) | op == lamOp = tyLam e1 e2
     alg (Ann _ (MkTuple es)) = tupleCon es
     alg (Ann loc _) = error ("Invalid type signature at " ++ show loc)

fromExprToPreds :: SynExp -> Set Pred
fromExprToPreds = fromList . f
 where 
  f (In (Ann _ (InfixApp op (In (Ann _ (Var n))) e2))) | op == juxtaOp = [IsIn n (fromExprToType e2)]
  f (In (Ann _ (MkTuple es))) = es >>= f
  f (In (Ann loc _)) = error ("Invalid type signature at " ++ show loc)

fromExprToQualType :: SynExp -> Qual Type
fromExprToQualType (In (Ann _ (InfixApp op e1 e2))) | op == classOp = fromExprToPreds e1 :=> fromExprToType e2
fromExprToQualType e = fromList [] :=> fromExprToType e

    