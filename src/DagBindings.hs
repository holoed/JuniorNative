module DagBindings where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF (Let, Let))
import Data.Set (Set, toList)
import FreeVariables (freeVars)
import qualified Data.Graph as G
import Data.List ( intersect, groupBy )

getName :: Exp -> String
getName (In (Let s _ _)) = s
getName _ = error "Expected a let binding"

getDeps :: Set String -> Exp -> [String]
getDeps globals e = toList xs
    where (In (Ann xs (Let _ _ _))) = freeVars globals e

deps :: Set String -> [Exp] -> [(String, [String])]
deps globals = foldl (\acc x -> (getName x, getDeps globals x) : acc) []

chunks :: Set String -> [Exp] -> [[(String, [String])]]
chunks globals es = reverse $ groupBy (\x y -> intersect (snd x) (snd y) == snd x) sg
    where (g, f, _) = (G.graphFromEdges . ((\(x, y) -> (x, x, y)) <$>) . deps globals) es
          sg = (\(_, y, z) -> (y, z)) . f <$> G.topSort g