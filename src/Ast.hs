{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Ast where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Annotations ( Ann(..) )

data Loc = Loc !Int  -- absolute character offset
               !Int  -- line number
               !Int  -- column number 
               deriving Eq

instance Show Loc where 
    show (Loc _ l c) = "line " ++ show l ++ " column " ++ show c

data ExpLoc = LitLoc Loc
            | VarLoc Loc
            | TupleLoc Loc
            | AppLoc
            | LamLoc Loc Loc
            | LetLoc Loc Loc
            | IfThenElseLoc Loc
            deriving (Eq, Show)

extractLoc :: ExpLoc -> Loc
extractLoc (VarLoc l) = l
extractLoc _ = error "undefined"

data ExpF a = Lit Prim
            | Var String
            | MkTuple [a]
            | App a a
            | Lam String a
            | Let String a a
            | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type Exp = Fix (Ann ExpLoc ExpF)

lit :: Loc -> Prim -> Exp
lit l v = In (Ann (LitLoc l) (Lit v))

var :: Loc -> String -> Exp
var l s = In (Ann (VarLoc l) (Var s))

app :: Exp -> Exp -> Exp
app e1 e2 = In (Ann AppLoc (App e1 e2))

lam :: Loc -> (String, Loc) -> Exp -> Exp
lam l (s, l') e = In (Ann (LamLoc l l') (Lam s e))

leT :: Loc -> (String, Loc) -> Exp -> Exp -> Exp
leT l (s, l') v b = In (Ann (LetLoc l l') (Let s v b))

ifThenElse :: Loc -> Exp -> Exp -> Exp -> Exp
ifThenElse l p e1 e2 = In (Ann (IfThenElseLoc l) (IfThenElse p e1 e2))

mkTuple :: Loc -> [Exp] -> Exp
mkTuple l xs = In (Ann (TupleLoc l) (MkTuple xs))
