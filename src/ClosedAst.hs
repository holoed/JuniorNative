{-# LANGUAGE TypeOperators #-}
module ClosedAst where

import Ast
import CoProduct

data ClosureF a = LookupEnv a Int
                | MakeEnv [a]
                | MakeClosure a a

type ClosedExpF = ExpF :+: ClosureF
