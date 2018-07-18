{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeOperators #-}
module ClosedAst where

import Ast
import CoProduct
import Fixpoint

data ClosureF a = LookupEnv a Int
                | MakeEnv [a]
                | ClosedLam String String a
                | MakeClosure a a deriving (Show, Eq, Functor, Traversable, Foldable)

type ClosedExpF = ExpF :+: ClosureF

type ClosedExp = Fix ClosedExpF

mkClosure :: ClosedExp -> ClosedExp -> ClosedExp
mkClosure env clam = In (Inr (MakeClosure env clam))

mkEnv :: [ClosedExp] -> ClosedExp
mkEnv vars = In (Inr (MakeEnv vars))

cLam :: String -> String -> ClosedExp -> ClosedExp
cLam env s e = In (Inr (ClosedLam env s e))

cVar :: String -> ClosedExp
cVar s = In (Inl (Var s))
