module DependencyAnalysis where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF (Defn, VarPat))
import Data.Set (Set, toList)
import Data.List (groupBy, partition)
import FreeVariables (freeVars)
import qualified Data.Graph as G

getName :: Exp -> String
getName (In (Ann _ (Defn _ (In (Ann _ (VarPat s))) _))) = s
getName _ = error "Expected a let binding"

getDeps :: Set String -> Exp -> [String]
getDeps globals e = toList (snd xs)
    where (In (Ann xs (Defn _ _ _))) = freeVars globals e           

deps :: Set String -> [Exp] -> [(String, [String])]
deps globals = foldl (\acc x -> (getName x, getDeps globals x) : acc) []

groupLayers :: [(String, [String])] -> [(String, [String])]
groupLayers xs = 
    if null sg4 || null sg3 then xs else pivot ++ sg3 ++ groupLayers sg4
    where
     pivot = take 1 xs
     pivotDeps = pivot >>= snd
     (sg3, sg4) = partition (\(_, xs') -> xs' == pivotDeps) (drop 1 xs)
     

chunks :: Set String -> [Exp] -> [[(String, [String])]]
chunks globals es = groupBy (\x y -> snd x == snd y) final
    where (g, f, _) = (G.graphFromEdges . ((\(x, y) -> (x, x, y)) <$>) . deps globals) es
          sg = reverse $ (\(_, y, z) -> (y, z)) . f <$> G.topSort g
          (sg1, sg2) = partition (\(_, xs) -> null xs) sg
          final = sg1 ++ groupLayers sg2