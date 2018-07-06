{-# LANGUAGE TypeOperators #-}
module ClosureConversion.ClosedAst where

import Ast
import CoProduct

data ClosureF a = LookupEnv a Int
                | MakeEnv [a]
                | MakeClosure a a

type ClosedAst = ExpF :+: ClosureF
