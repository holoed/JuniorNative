module Junior.TypeChecker.TypedAst where

import Junior.Parser.Location ( Loc )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Core.Types ( Type, Qual )
import Junior.Utils.Annotations ( Ann(..) )
import Junior.Parser.Primitives ( Prim )
import Junior.Core.Ast (ExpF(Lit, Var, VarPat, App, Lam, Let, IfThenElse, MkTuple, TuplePat, Defn, Match, MatchExp, ConPat, LitPat), ExpF)

type TypedExpF = Ann (Maybe Loc, Qual Type) ExpF
type TypedExp = Fix TypedExpF

tlit :: Loc -> Qual Type -> Prim -> TypedExp
tlit l t v = In (Ann (Just l, t) (Lit v))

tvar :: Loc -> Qual Type -> String -> TypedExp
tvar l t s = In (Ann (Just l, t) (Var s))

tvarPat :: Loc -> Qual Type -> String -> TypedExp
tvarPat l t s = In (Ann (Just l, t) (VarPat s))

tlitPat :: Loc -> Qual Type -> Prim -> TypedExp
tlitPat l t x = In (Ann (Just l, t) (LitPat x))

tapp :: Qual Type -> TypedExp -> TypedExp -> TypedExp
tapp t e1 e2 = In (Ann (Nothing , t) (App e1 e2))

tlam :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp
tlam l t p e = In (Ann (Just l, t) (Lam p e))

tleT :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tleT l t ps v b = In (Ann (Just l, t) (Let ps v b))

tifThenElse :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp -> TypedExp
tifThenElse l t p e1 e2 = In (Ann (Just l, t) (IfThenElse p e1 e2))

tmatch :: Loc -> Qual Type -> TypedExp -> [TypedExp] -> TypedExp
tmatch l t e es = In (Ann (Just l, t) (Match e es))

tmatchExp :: Loc -> Qual Type -> TypedExp -> TypedExp -> TypedExp
tmatchExp l t e1 e2 = In (Ann (Just l, t) (MatchExp e1 e2))

tmkTuple :: Loc -> Qual Type -> [TypedExp] -> TypedExp
tmkTuple l t xs = In (Ann (Just l, t) (MkTuple xs))

ttuplePat :: Loc -> Qual Type -> [TypedExp] -> TypedExp
ttuplePat l t xs = In (Ann (Just l, t) (TuplePat xs))

tconPat :: Loc -> Qual Type -> String -> [TypedExp] -> TypedExp
tconPat l t n xs = In (Ann (Just l, t) (ConPat n xs))

tdefn :: Loc -> Qual Type -> Maybe (Qual Type) -> TypedExp -> TypedExp -> TypedExp
tdefn l t t2 ps v = In (Ann (Just l, t) (Defn t2 ps v))