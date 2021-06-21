{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module PAst where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Operators ( Operator )

data Loc = Loc !Int  -- absolute character offset
               !Int  -- line number
               !Int  -- column number

zeroLoc :: Loc
zeroLoc = Loc 0 0 0

data SynExpF a = Lit Prim
               | Var String
               | MkTuple [a]
               | App a a
               | InfixApp Operator a a
               | Lam [String] a
               | Let [String] a a
               | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type SynExp = Fix SynExpF

lit :: Loc -> Prim -> SynExp
lit l v = In (Lit v)

var :: Loc -> String -> SynExp
var l s = In (Var s)

app :: SynExp -> SynExp -> SynExp
app e1 e2 = In (App e1 e2)

infixApp :: Loc -> Operator -> SynExp -> SynExp -> SynExp
infixApp l op e1 e2 = In (InfixApp op e1 e2)

lam :: Loc -> [String] -> SynExp -> SynExp
lam l s e = In (Lam s e)

leT :: [String] -> SynExp -> SynExp -> SynExp
leT s v b = In (Let s v b)

ifThenElse :: Loc -> SynExp -> SynExp -> SynExp -> SynExp
ifThenElse l p e1 e2 = In (IfThenElse p e1 e2)

mkTuple :: Loc -> [SynExp] -> SynExp
mkTuple l xs = In (MkTuple xs)

defn :: Loc -> [String] -> SynExp -> SynExp
defn l s v = In (Let s v (var l $ head s))
