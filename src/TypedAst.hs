module TypedAst where

import Fixpoint ( Fix(In) )
import Types ( Type, Qual )
import Annotations ( Ann(..) )
import Primitives ( Prim )
import Ast (ExpF(Lit, Var, VarPat, App, Lam, Let, IfThenElse, MkTuple, TuplePat), ExpF, Loc)

type TypedExp = Fix (Ann (Maybe Loc, Qual Type) ExpF)

tlit :: Loc -> Qual Type -> Prim -> TypedExp
tlit l t v = In (Ann (Just l, t) (Lit v))

tvar :: Loc -> Qual Type -> String -> TypedExp
tvar l t s = In (Ann (Just l, t) (Var s))

tvarPat :: Loc -> Qual Type -> String -> TypedExp
tvarPat l t s = In (Ann (Just l, t) (VarPat s))

tapp :: Qual Type -> TypedExp -> TypedExp -> TypedExp
tapp t e1 e2 = In (Ann (Nothing , t) (App e1 e2))

tlam :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp
tlam l t p e = In (Ann (Just l, t) (Lam [p] e))

tleT :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tleT l t p v b = In (Ann (Just l, t) (Let [p] v b))

tifThenElse :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tifThenElse l t p e1 e2 = In (Ann (Just l, t) (IfThenElse p e1 e2))

tmkTuple :: Loc -> Qual Type -> [TypedExp] -> TypedExp
tmkTuple l t xs = In (Ann (Just l, t) (MkTuple xs))

ttuplePat :: Loc -> Qual Type -> [TypedExp] -> TypedExp
ttuplePat l t xs = In (Ann (Just l, t) (TuplePat xs))