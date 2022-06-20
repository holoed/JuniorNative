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

dotOp :: ([Char], Integer, Fixity)
dotOp = (".", 9, Infix Right)

mulOp :: ([Char], Integer, Fixity)
mulOp    = ("*", 7, Infix Left)

divOp :: ([Char], Integer, Fixity)
divOp    = ("/", 7, Infix Left)

plusOp :: ([Char], Integer, Fixity)
plusOp   = ("+", 6, Infix Left)

subOp :: ([Char], Integer, Fixity)
subOp    = ("-", 6, Infix Left)

ltGtOp :: ([Char], Integer, Fixity)
ltGtOp = ("<>", 6, Infix Right)

plusplusOp :: ([Char], Integer, Fixity)
plusplusOp = ("++", 5, Infix Right) 

ltStarGtOp :: ([Char], Integer, Fixity)
ltStarGtOp = ("<*>", 4, Infix Left)

ltDollarGtOp :: ([Char], Integer, Fixity)
ltDollarGtOp = ("<$>", 4, Infix Left)

consOp :: ([Char], Integer, Fixity)
consOp = (":", 5, Infix Right) 

eqeqOp :: ([Char], Integer, Fixity)
eqeqOp   = ("==", 4, Infix NonAssoc) 

noteqOp :: ([Char], Integer, Fixity)
noteqOp   = ("/=", 4, Infix NonAssoc) 

gteqOp :: ([Char], Integer, Fixity)
gteqOp   = (">=", 4, Infix NonAssoc) 

lteqOp :: ([Char], Integer, Fixity)
lteqOp   = ("<=", 4, Infix NonAssoc) 

gtOp :: ([Char], Integer, Fixity)
gtOp     = (">", 4, Infix NonAssoc)

ltOp :: ([Char], Integer, Fixity)
ltOp     = ("<",  4, Infix NonAssoc)

andOp :: ([Char], Integer, Fixity)
andOp = ("&&", 3, Infix Right)

orOp :: ([Char], Integer, Fixity)
orOp = ("||", 2, Infix Right)  

gtEqGtOp :: ([Char], Integer, Fixity)
gtEqGtOp = (">=>", 1, Infix Right)

gtGtEqOp :: ([Char], Integer, Fixity)
gtGtEqOp = (">>=", 1, Infix Left)

lamOp :: ([Char], Integer, Fixity)
lamOp    = ("->", -1, Infix Right)

classOp :: ([Char], Integer, Fixity)
classOp  = ("=>", -2, Infix Right)

minOp :: ([Char], Integer, Fixity)
minOp    = ("<minimum-precedence-operator>", -99, Infix NonAssoc)
