{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Ast where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Annotations ( Ann(..) )
import Location (Loc)
import Types ( Qual, Type )
import TypesPrinter ()

data TypeDecl = TypeDecl Type [Type] [String]

data ExpF a = Lit Prim
            | Var String
            | VarPat String
            | MkTuple [a]
            | TuplePat [a]
            | App a a
            | Lam a a
            | Let a a a
            | IfThenElse a a a 
            | Defn (Maybe (Qual Type)) a a 
            {- This section is for Closure Conversion -}
            | MkClosure String 
            | SetEnv String a a
            | GetEnv String a 
            | AppClosure a a deriving (Show, Eq, Functor, Traversable, Foldable)

type Exp = Fix (Ann (Maybe Loc) ExpF)

lit :: Loc -> Prim -> Exp
lit l v = In (Ann (Just l) (Lit v))

var :: Loc -> String -> Exp
var l s = In (Ann (Just l) (Var s))

varPat :: Loc -> String -> Exp
varPat l s = In (Ann (Just l) (VarPat s))

app :: Exp -> Exp -> Exp
app e1 e2 = In (Ann Nothing  (App e1 e2))

lam :: Loc -> Exp -> Exp -> Exp
lam l p e = In (Ann (Just l) (Lam p e))

leT :: Loc -> Exp -> Exp -> Exp -> Exp
leT l ps v b = In (Ann (Just l) (Let ps v b))

ifThenElse :: Loc -> Exp -> Exp -> Exp -> Exp
ifThenElse l p e1 e2 = In (Ann (Just l) (IfThenElse p e1 e2))

mkTuple :: Loc -> [Exp] -> Exp
mkTuple l xs = In (Ann (Just l) (MkTuple xs))

tuplePat :: Loc -> [Exp] -> Exp
tuplePat l xs = In (Ann (Just l) (TuplePat xs))

defn :: Loc -> Maybe (Qual Type) -> Exp -> Exp -> Exp
defn l qt p v = In (Ann (Just l) (Defn qt p v))

extractNameFromPat :: Exp -> (Maybe Loc, String) 
extractNameFromPat (In (Ann l (VarPat s))) = (l, s)
extractNameFromPat _ = error "Unsupported"
