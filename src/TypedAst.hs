module TypedAst where

import Fixpoint
import Types
import Annotations
import Operators
import Ast (ExpF(Lit, Var, App, InfixApp, Lam, Let, IfThenElse, MkTuple), ExpF, Prim)

type TypedExp = Fix (Ann (Qual Type) ExpF)

tlit :: Qual Type -> Prim -> TypedExp
tlit t v = In (Ann t (Lit v))

tvar :: Qual Type -> String -> TypedExp
tvar t s = In (Ann t (Var s))

tapp :: Qual Type -> TypedExp -> TypedExp -> TypedExp
tapp t e1 e2 = In (Ann t (App e1 e2))

tinfixApp :: Qual Type -> Operator -> TypedExp -> TypedExp -> TypedExp
tinfixApp t op e1 e2 = In (Ann t (InfixApp op e1 e2))

tlam :: Qual Type -> String -> TypedExp -> TypedExp
tlam t s e = In (Ann t (Lam s e))

tleT :: Qual Type -> String -> TypedExp -> TypedExp -> TypedExp
tleT t s v b = In (Ann t (Let s v b))

tifThenElse :: Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tifThenElse t p e1 e2 = In (Ann t (IfThenElse p e1 e2))

tmkTuple :: Qual Type -> [TypedExp] -> TypedExp
tmkTuple t xs = In (Ann t (MkTuple xs))