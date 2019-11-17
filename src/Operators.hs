module Operators where

type Precedence = Integer

data Associativity = LeftAssoc | RightAssoc | NonAssoc deriving (Eq, Show)
    
type Operator = (String, Precedence, Associativity)
    
juxtaOp  = (" ", 20, LeftAssoc)
mulOp    = ("*", 15, LeftAssoc)
divOp    = ("/", 15, LeftAssoc)
plusOp   = ("+", 14, LeftAssoc)
subOp    = ("-", 14, LeftAssoc)
eqeqOp   = ("==", 11, LeftAssoc) 