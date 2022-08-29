{-# LANGUAGE TupleSections #-}
module Junior.Parser.SynExpToTypeDecl where

import qualified Junior.Core.Ast as Ast
import qualified Junior.Parser.PAst as PAst
import Junior.Utils.Annotations ( Ann(Ann) )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Pretty.TypesPrinter () 
import Junior.TypeChecker.Environment (Env, toEnv)
import qualified Data.Set as Set
import Junior.Core.Types ( Qual((:=>)), Type (TyCon, TyApp), tyLam, Pred (IsIn) ) 
import Junior.TypeChecker.ContextReduction (ClassEnv (ClassEnv, classes, defaults))
import Data.Map (update)

toTypeDecl :: PAst.SynExp -> [Ast.TypeDecl]
toTypeDecl (In (Ann _ (PAst.TypeDecl t ts ds)))= [Ast.TypeDecl t ts ds]
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
toDefns (TyApp (TyApp (TyApp (TyApp (TyCon n1) t1) t2) t3) t4) t = 
    [(n1,Set.fromList [] :=> tyLam t1 (tyLam t2 (tyLam t3 (tyLam t4 t)))), 
     ("is" <> n1, Set.fromList [] :=> tyLam t (TyCon "Bool")),
     ("extract" <> n1, Set.fromList [] :=> tyLam t (TyApp (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) t4))]
toDefns t1 _ = error $ "Unknown data type declaration " <> show t1

fromTypeDeclToEnv :: Ast.TypeDecl -> Env 
fromTypeDeclToEnv (Ast.TypeDecl t ts _) =  
    toEnv $ ts >>= (`toDefns` t) 

fromTypeDeclToClassEnv :: Ast.TypeDecl -> ClassEnv -> ClassEnv
fromTypeDeclToClassEnv (Ast.TypeDecl (TyApp (TyCon n) _) _ [name]) classEnv =
    let classes' = classes classEnv in
    ClassEnv { classes = update (\(xs, ys) -> Just (xs, ys <> [Set.fromList [] :=> IsIn name (TyCon n)])) name classes', defaults = defaults classEnv  }
fromTypeDeclToClassEnv (Ast.TypeDecl (TyApp (TyApp (TyCon n) t) _) _ [name]) classEnv =
    let classes' = classes classEnv in
    ClassEnv { classes = update (\(xs, ys) -> Just (xs, ys <> [Set.fromList [] :=> IsIn name (TyApp (TyCon n) t)])) name classes', defaults = defaults classEnv  }
fromTypeDeclToClassEnv _ x = x