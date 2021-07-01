{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Ast where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Annotations ( Ann(..) )
import Location (Loc)

data ExpF a = Lit Prim
            | Var String
            | VarPat String
            | MkTuple [a]
            | TuplePat [a]
            | App a a
            | Lam a a
            | Let a a a
            | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type Exp = Fix (Ann (Maybe Loc) ExpF)

lit :: Loc -> Prim -> Exp
lit l v = In (Ann (Just l) (Lit v))

var :: Loc -> String -> Exp
var l s = In (Ann (Just l) (Var s))

varPat :: Loc -> String -> Exp
varPat l s = In (Ann (Just l) (VarPat s))

app :: Exp -> Exp -> Exp
app e1 e2 = In (Ann Nothing  (App e1 e2))

lam :: Loc -> Exp -> Exp -> Exp
lam l p e = In (Ann (Just l) (Lam p e))

leT :: Loc -> Exp -> Exp -> Exp -> Exp
leT l ps v b = In (Ann (Just l) (Let ps v b))

ifThenElse :: Loc -> Exp -> Exp -> Exp -> Exp
ifThenElse l p e1 e2 = In (Ann (Just l) (IfThenElse p e1 e2))

mkTuple :: Loc -> [Exp] -> Exp
mkTuple l xs = In (Ann (Just l) (MkTuple xs))

tuplePat :: Loc -> [Exp] -> Exp
tuplePat l xs = In (Ann (Just l) (TuplePat xs))
