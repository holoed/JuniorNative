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
                | MakeClosure a a deriving (Show, Eq, Functor, Traversable, Foldable)

type ClosedExpF = ExpF :+: ClosureF

type ClosedExp = Fix ClosedExpF
