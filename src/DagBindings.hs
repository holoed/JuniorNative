module DagBindings where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF (Let, Let))
import Data.Set (toList)
import FreeVariables (freeVars)
import qualified Data.Graph as G
import Data.List ( groupBy )

getName :: Exp -> String
getName (In (Let s _ _)) = s
getName _ = error "Expected a let binding"

getDeps :: Exp -> [String]
getDeps e = toList xs
    where (In (Ann xs (Let _ _ _))) = freeVars e

deps :: [Exp] -> [(String, [String])]
deps = foldl (\acc x -> (getName x, getDeps x) : acc) []

chunks :: [Exp] -> [[String]]
chunks es = reverse $ (fst <$>) <$> groupBy (\x y -> snd x == snd y) sg
    where (g, f, _) = (G.graphFromEdges . ((\(x, y) -> (x, x, y)) <$>) . deps) es
          sg = (\(_, y, z) -> (y, z)) . f <$> G.topSort g