module Operators where

import Prelude hiding (Left, Right)

type Precedence = Integer

data Associativity = Left | Right | NonAssoc deriving (Eq, Show)
    
data Fixity = Prefix | Postfix | Infix Associativity deriving (Eq, Show)

type Operator = (String, Precedence, Fixity)

maxOp :: ([Char], Integer, Fixity)
maxOp    = ("<maximum-precedence-operator>", 99, Infix NonAssoc) 

juxtaOp :: ([Char], Integer, Fixity)
juxtaOp  = (" ", 20, Infix Left)

mulOp :: ([Char], Integer, Fixity)
mulOp    = ("*", 15, Infix Left)

divOp :: ([Char], Integer, Fixity)
divOp    = ("/", 15, Infix Left)

plusOp :: ([Char], Integer, Fixity)
plusOp   = ("+", 14, Infix Left)

subOp :: ([Char], Integer, Fixity)
subOp    = ("-", 14, Infix Left)

eqeqOp :: ([Char], Integer, Fixity)
eqeqOp   = ("==", 11, Infix Left) 

gtOp :: ([Char], Integer, Fixity)
gtOp     = (">", 10, Infix Left)

ltOp :: ([Char], Integer, Fixity)
ltOp     = ("<",  9, Infix Left)

lamOp :: ([Char], Integer, Fixity)
lamOp    = ("->", 8, Infix Right)

minOp :: ([Char], Integer, Fixity)
minOp    = ("<minimum-precedence-operator>", 0, Infix NonAssoc)
