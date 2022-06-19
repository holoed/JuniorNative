{-# LANGUAGE TupleSections #-}
module SynExpToTypeDecl where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import TypesPrinter () 
import Environment (Env, toEnv)
import qualified Data.Set as Set
import Types ( Qual((:=>)), Type (TyCon, TyApp), tyLam ) 

toTypeDecl :: PAst.SynExp -> [Ast.TypeDecl]
toTypeDecl (In (Ann _ (PAst.TypeDecl t ts)))= [Ast.TypeDecl t ts]
toTypeDecl _ = []

toDefns :: Type -> Type -> [(String, Qual Type)]
toDefns (TyCon n1) t = 
    [(n1, Set.fromList [] :=> t), 
     ("is" <> n1, Set.fromList [] :=> tyLam t (TyCon "Bool"))]
toDefns (TyApp (TyCon n1) t1) t = 
    [(n1,Set.fromList [] :=> tyLam t1 t), 
     ("is" <> n1, Set.fromList [] :=> tyLam t (TyCon "Bool")),
     ("extract" <> n1, Set.fromList [] :=> tyLam t t1)]
toDefns (TyApp (TyApp (TyCon n1) t1) t2) t = 
    [(n1,Set.fromList [] :=> tyLam t1 (tyLam t2 t)), 
     ("is" <> n1, Set.fromList [] :=> tyLam t (TyCon "Bool")),
     ("extract" <> n1, Set.fromList [] :=> tyLam t (TyApp (TyApp (TyCon "Tuple") t1) t2))]
toDefns (TyApp (TyApp (TyApp (TyCon n1) t1) t2) t3) t = 
    [(n1,Set.fromList [] :=> tyLam t1 (tyLam t2 (tyLam t3 t))), 
     ("is" <> n1, Set.fromList [] :=> tyLam t (TyCon "Bool")),
     ("extract" <> n1, Set.fromList [] :=> tyLam t (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3))]
toDefns _ _ = error "Unknown data type declaration"

fromTypeDeclToEnv :: Ast.TypeDecl -> Env 
fromTypeDeclToEnv (Ast.TypeDecl t ts) =  
    toEnv $ ts >>= (`toDefns` t) 

