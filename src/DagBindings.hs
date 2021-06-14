module DagBindings where

import Fixpoint ( Fix(In) ) 
import Annotations ( Ann(Ann) ) 
import Ast (Exp, ExpF (Let, Lit, Var, MkTuple, App, Lam, Let))
import Data.Set (toList)
import Data.List (foldl)
import RecursionSchemes ( cataRec )
import FreeVariables (freeVars)
import qualified Data.Graph as G

getName :: Exp -> String 
getName (In (Let s _ _)) = s
getName _ = error "Expected a let binding"

getDeps :: Exp -> [String]
getDeps e = toList xs
    where (In (Ann xs (Let _ _ _))) = freeVars e 

create :: [Exp] -> [(String, [String])]
create = foldl (\acc x -> (getName x, getDeps x) : acc) [] 