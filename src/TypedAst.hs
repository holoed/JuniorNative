module TypedAst where

import Fixpoint ( Fix(In) )
import Types ( Type, Qual )
import Annotations ( Ann(..) )
import Primitives ( Prim )
import Ast (ExpF(Lit, Var, App, Lam, Let, IfThenElse, MkTuple), ExpF, ExpLoc(..), Loc)

type TypedExp = Fix (Ann (ExpLoc, Qual Type) ExpF)

tlit :: Loc -> Qual Type -> Prim -> TypedExp
tlit l t v = In (Ann (LitLoc l, t) (Lit v))

tvar :: Loc -> Qual Type -> String -> TypedExp
tvar l t s = In (Ann (VarLoc l, t) (Var s))

tapp :: Qual Type -> TypedExp -> TypedExp -> TypedExp
tapp t e1 e2 = In (Ann (AppLoc, t) (App e1 e2))

tlam :: Loc -> Qual Type -> (String, Loc) -> TypedExp -> TypedExp
tlam l t (s, l') e = In (Ann (LamLoc l l', t) (Lam s e))

tleT :: Loc -> Qual Type -> (String, Loc) -> TypedExp -> TypedExp -> TypedExp
tleT l t (s, l') v b = In (Ann (LetLoc l l', t) (Let s v b))

tifThenElse :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tifThenElse l t p e1 e2 = In (Ann (IfThenElseLoc l, t) (IfThenElse p e1 e2))

tmkTuple :: Loc -> Qual Type -> [TypedExp] -> TypedExp
tmkTuple l t xs = In (Ann (TupleLoc l, t) (MkTuple xs))