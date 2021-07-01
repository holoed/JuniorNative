{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PAst where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Operators ( Operator )
import Annotations ( Ann(..) )

data Loc = Loc !Int  -- absolute character offset
               !Int  -- line number
               !Int  -- column number 
               deriving Show

data SynExpF a = Lit Prim
               | Var String
               | VarPat String
               | MkTuple [a]
               | TuplePat [a]
               | App a a
               | InfixApp Operator a a
               | Lam [a] a
               | Let [a] a a
               | IfThenElse a a a deriving (Show, Eq, Functor, Traversable, Foldable)

type SynExp = Fix (Ann (Maybe Loc) SynExpF)

lit :: Loc -> Prim -> SynExp
lit l v = In (Ann (Just l) (Lit v))

var :: Loc -> String -> SynExp
var l s = In (Ann (Just l) (Var s))

varPat :: Loc -> String -> SynExp
varPat l s = In (Ann (Just l) (VarPat s))

tuplePat :: Loc ->[SynExp] -> SynExp
tuplePat l xs = In (Ann (Just l) (TuplePat xs))

app :: SynExp -> SynExp -> SynExp
app e1 e2 = In (Ann Nothing (App e1 e2))

infixApp :: Loc -> Operator -> SynExp -> SynExp -> SynExp
infixApp l op e1 e2 = In (Ann (Just l) (InfixApp op e1 e2))

lam :: Loc -> [SynExp] -> SynExp -> SynExp
lam l s e = In (Ann (Just l) (Lam s e))

leT :: Loc -> [SynExp] -> SynExp -> SynExp -> SynExp
leT l s v b = In (Ann (Just l) (Let s v b))

ifThenElse :: Loc -> SynExp -> SynExp -> SynExp -> SynExp
ifThenElse l p e1 e2 = In (Ann (Just l) (IfThenElse p e1 e2))

mkTuple :: Loc -> [SynExp] -> SynExp
mkTuple l xs = In (Ann (Just l) (MkTuple xs))

defn :: Loc -> [SynExp] -> SynExp -> SynExp
defn l (p@(In (Ann (Just l') (VarPat s))):ps) v = In (Ann (Just l) (Let (p:ps) v (var l' s)))
