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

toDefn :: Type -> Type -> (String, Qual Type)
toDefn (TyCon n1) t = (n1, Set.fromList [] :=> t) 
toDefn (TyApp (TyCon n1) t1) t = (n1,Set.fromList [] :=> tyLam t1 t)
toDefn _ _ = error "Unknown data type declaration"

fromTypeDeclToEnv :: Ast.TypeDecl -> Env 
fromTypeDeclToEnv (Ast.TypeDecl t ts) =  
    toEnv $ (`toDefn` t) <$> ts

