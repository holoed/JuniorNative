{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Junior.Core.Ast where

import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Parser.Primitives ( Prim )
import Junior.Utils.Annotations ( Ann(..) )
import Junior.Parser.Location (Loc)
import Junior.Core.Types ( Qual, Type )
import Junior.Pretty.TypesPrinter ()

data TypeDecl = TypeDecl Type [Type] [String]

data ExpF a = Lit Prim
            | Var String
            | VarPat String
            | LitPat Prim
            | MkTuple [a]
            | TuplePat [a]
            | ConPat String [a]
            | App a a
            | Lam a a
            | Let a a a
            | IfThenElse a a a 
            | Match a [a]
            | MatchExp a a
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

litPat :: Loc -> Prim -> Exp
litPat l x = In (Ann (Just l) (LitPat x))

app :: Exp -> Exp -> Exp
app e1 e2 = In (Ann Nothing  (App e1 e2))

lam :: Loc -> Exp -> Exp -> Exp
lam l p e = In (Ann (Just l) (Lam p e))

leT :: Loc -> Exp -> Exp -> Exp -> Exp
leT l ps v b = In (Ann (Just l) (Let ps v b))

ifThenElse :: Loc -> Exp -> Exp -> Exp -> Exp
ifThenElse l p e1 e2 = In (Ann (Just l) (IfThenElse p e1 e2))

matcH :: Loc -> Exp -> [Exp] -> Exp
matcH l e es = In (Ann (Just l) (Match e es))

matchExp :: Loc -> Exp -> Exp -> Exp
matchExp l e1 e2 = In (Ann (Just l) (MatchExp e1 e2))

mkTuple :: Loc -> [Exp] -> Exp
mkTuple l xs = In (Ann (Just l) (MkTuple xs))

tuplePat :: Loc -> [Exp] -> Exp
tuplePat l xs = In (Ann (Just l) (TuplePat xs))

conPat :: Loc -> String -> [Exp] -> Exp
conPat l name xs = In (Ann (Just l) (ConPat name xs))

defn :: Loc -> Maybe (Qual Type) -> Exp -> Exp -> Exp
defn l qt p v = In (Ann (Just l) (Defn qt p v))

extractNameFromPat :: Exp -> (Maybe Loc, String) 
extractNameFromPat (In (Ann l (VarPat s))) = (l, s)
extractNameFromPat _ = error "Unsupported"
