module FreeVariables where

import Ast
import RecursionSchemes
import qualified Data.Set as Set

alg :: ExpF (Set.Set String) -> Set.Set String
alg (Lit _) = Set.empty
alg (Var s) = Set.singleton s
alg (MkTuple xs) = Set.unions xs
alg (App e1 e2) = Set.union e1 e2
alg (Lam s e) = Set.delete s e
alg (Let s e1 e2) = Set.delete s (e1 `Set.union` e2) 

freeVars :: Exp -> Set.Set String
freeVars = cataRec alg
