module DagBindings where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF (Let, VarPat))
import Data.Set (Set, toList)
import Data.List (groupBy)
import FreeVariables (freeVars)
import qualified Data.Graph as G

getName :: Exp -> String
getName (In (Ann _ (Let [In (Ann _ (VarPat s))] _ _))) = s
getName _ = error "Expected a let binding"

getDeps :: Set String -> Exp -> [String]
getDeps globals e = toList (snd xs)
    where (In (Ann xs (Let _ _ _))) = freeVars globals e

deps :: Set String -> [Exp] -> [(String, [String])]
deps globals = foldl (\acc x -> (getName x, getDeps globals x) : acc) []

chunks :: Set String -> [Exp] -> [[(String, [String])]]
chunks globals es = groupBy (\x y -> snd x == snd y) (reverse sg)
    where (g, f, _) = (G.graphFromEdges . ((\(x, y) -> (x, x, y)) <$>) . deps globals) es
          sg = (\(_, y, z) -> (y, z)) . f <$> G.topSort g