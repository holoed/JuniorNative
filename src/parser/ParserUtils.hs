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
import Location (Loc, PString (PStr))
import Control.Monad.Except ( Except )
import Control.Monad.Trans.Except (throwE)

fromExprToType :: SynExp -> Except PString Type
fromExprToType = cataRec alg 
    where
     alg :: Ann (Maybe Loc) SynExpF (Except PString Type) -> Except PString Type
     alg (Ann _ (Var n)) | isLower (head n) = return $ TyVar n 0
     alg (Ann _ (Var n)) = return $ TyCon n
     alg (Ann _ (InfixApp op e1 e2)) | op == juxtaOp = 
          do e1' <- e1
             e2' <- e2
             return $ TyApp e1' e2'
     alg (Ann _ (InfixApp op e1 e2)) | op == lamOp = 
          do e1' <- e1
             e2' <- e2
             return $ tyLam e1' e2'
     alg (Ann _ (MkTuple es)) = tupleCon <$> (sequence es)  
     alg (Ann loc _) = throwE (PStr ("Invalid type signature at ",  loc))

fromExprToPreds :: SynExp -> Except PString (Set Pred)
fromExprToPreds e = fromList <$> f e
 where 
  f :: SynExp -> Except PString [Pred]
  f (In (Ann _ (InfixApp op (In (Ann _ (Var n))) e2))) | op == juxtaOp = (\x -> [IsIn n x]) <$> fromExprToType e2
  f (In (Ann _ (MkTuple es))) = (\xss -> xss >>= id) <$> sequence (f <$> es) 
  f (In (Ann loc _)) = throwE (PStr ("Invalid type signature at ",  loc))

fromExprToQualType :: SynExp -> Except PString (Qual Type)
fromExprToQualType (In (Ann _ (InfixApp op e1 e2))) | op == classOp = 
    do e1' <- fromExprToPreds e1 
       e2' <- fromExprToType e2
       return $ e1' :=> e2'
fromExprToQualType e = do e' <- fromExprToType e
                          return $ fromList [] :=> e' 

    