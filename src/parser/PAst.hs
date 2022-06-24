{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PAst where

import Fixpoint ( Fix(In) )
import Primitives ( Prim )
import Operators ( Operator, Fixity (Infix), Associativity(Right) )
import Annotations ( Ann(..) )
import Location (Loc)
import Types ( Qual, Type )
import TypesPrinter () 

data SynExpF a = Lit Prim
               | Var String
               | VarPat String
               | LitPat Prim
               | MkTuple [a]
               | TuplePat [a]
               | ConPat String [a]
               | App a a
               | InfixApp Operator a a
               | Lam [a] a
               | Let [a] a a
               | IfThenElse a a a 
               | Match a [a]
               | MatchExp a a
               | Defn (Maybe (Qual Type)) [a] a 
               | TypeDecl Type [Type] [String] deriving (Show, Eq, Functor, Traversable, Foldable)

type SynExp = Fix (Ann (Maybe Loc) SynExpF)

lit :: Loc -> Prim -> SynExp
lit l v = In (Ann (Just l) (Lit v))

var :: Loc -> String -> SynExp
var l s = In (Ann (Just l) (Var s))

varPat :: Loc -> String -> SynExp
varPat l s = In (Ann (Just l) (VarPat s))

litPat :: Loc -> Prim -> SynExp
litPat l s = In (Ann (Just l) (LitPat s))

tuplePat :: Loc ->[SynExp] -> SynExp
tuplePat l xs = In (Ann (Just l) (TuplePat xs))

conPat :: Loc -> String ->[SynExp] -> SynExp
conPat l name xs = In (Ann (Just l) (ConPat name xs))

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

mkList :: Loc -> [SynExp] -> SynExp
mkList l xs = foldr f (In (Ann (Just l) (Var "[]"))) xs
    where f e1 e2 = In (Ann (Just l) (InfixApp (":",12, Infix (Operators.Right)) e1 e2))

defn :: Loc -> Maybe (Qual Type) -> [SynExp] -> SynExp -> SynExp
defn l qt ps v = In (Ann (Just l) (Defn qt ps v))

typeDecl :: Loc -> Type -> [Type] -> [String] -> SynExp
typeDecl l t ts ds = In (Ann (Just l) (TypeDecl t ts ds))

matcH :: Loc -> SynExp -> [SynExp] -> SynExp
matcH l e es = In (Ann (Just l) (Match e es))

patternMatch :: Loc -> SynExp -> SynExp -> SynExp
patternMatch l e1 e2 = In (Ann (Just l) (MatchExp e1 e2))
