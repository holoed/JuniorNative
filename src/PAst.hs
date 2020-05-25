{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module PAst where

import Fixpoint
import Primitives
import Operators

data Decl a = Decl [String] a

data SynExpF a = Lit Prim
               | Var String
               | MkTuple [a]
               | App a a
               | InfixApp Operator a a
               | Lam [String] a
               | Let [String] a a
               | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type SynExp = Fix SynExpF

lit :: Prim -> SynExp
lit v = In (Lit v)

var :: String -> SynExp
var s = In (Var s)

app :: SynExp -> SynExp -> SynExp
app e1 e2 = In (App e1 e2)

infixApp :: Operator -> SynExp -> SynExp -> SynExp
infixApp op e1 e2 = In (InfixApp op e1 e2)

lam :: [String] -> SynExp -> SynExp
lam s e = In (Lam s e)

leT :: [String] -> SynExp -> SynExp -> SynExp
leT s v b = In (Let s v b)

ifThenElse :: SynExp -> SynExp -> SynExp -> SynExp
ifThenElse p e1 e2 = In (IfThenElse p e1 e2)

mkTuple :: [SynExp] -> SynExp
mkTuple xs = In (MkTuple xs)