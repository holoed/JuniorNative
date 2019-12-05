{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Ast where

import Fixpoint
import Operators

data Prim = I Int | B Bool | S String deriving (Eq, Show)

data ExpF a = Lit Prim
            | Var String
            | MkTuple [a]
            | App a a
            | InfixApp Operator a a
            | Lam String a
            | Let String a a
            | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type Exp = Fix ExpF

lit :: Prim -> Exp
lit v = In (Lit v)

var :: String -> Exp
var s = In (Var s)

app :: Exp -> Exp -> Exp
app e1 e2 = In (App e1 e2)

infixApp :: Operator -> Exp -> Exp -> Exp
infixApp op e1 e2 = In (InfixApp op e1 e2)

lam :: String -> Exp -> Exp
lam s e = In (Lam s e)

leT :: String -> Exp -> Exp -> Exp
leT s v b = In (Let s v b)

ifThenElse :: Exp -> Exp -> Exp -> Exp
ifThenElse p e1 e2 = In (IfThenElse p e1 e2)

mkTuple :: [Exp] -> Exp
mkTuple xs = In (MkTuple xs)
