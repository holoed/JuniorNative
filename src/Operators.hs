module Operators where

import Prelude hiding (Left, Right)

type Precedence = Integer

data Associativity = Left | Right | NonAssoc deriving (Eq, Show)
    
data Fixity = Prefix | Postfix | Infix Associativity deriving (Eq, Show)

type Operator = (String, Precedence, Fixity)

maxOp    = ("<maximum-precedence-operator>", 99, Infix NonAssoc)              
juxtaOp  = (" ", 20, Infix Left)
mulOp    = ("*", 15, Infix Left)
divOp    = ("/", 15, Infix Left)
plusOp   = ("+", 14, Infix Left)
subOp    = ("-", 14, Infix Left)
eqeqOp   = ("==", 11, Infix Left) 
gtOp     = (">", 10, Infix Left)
ltOp     = ("<",  9, Infix Left)
lamOp    = ("->", 8, Infix Right)
minOp    = ("<minimum-precedence-operator>", 0, Infix NonAssoc)
