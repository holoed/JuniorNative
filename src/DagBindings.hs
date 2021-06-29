module DagBindings where

import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Ast (Exp, ExpF (Let, VarPat))
import Data.Set (Set, toList)
import Data.List (groupBy, partition)
import FreeVariables (freeVars)
import qualified Data.Graph as G

getName :: Exp -> String
getName (In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) = s
getName _ = error "Expected a let binding"

getDeps :: Set String -> Exp -> [String]
getDeps globals e = toList (snd xs)
    where (In (Ann xs (Let _ _ _))) = freeVars globals e

deps :: Set String -> [Exp] -> [(String, [String])]
deps globals = foldl (\acc x -> (getName x, getDeps globals x) : acc) []

foo :: [(String, [String])] -> [(String, [String])]
foo xs = final
    where
     pivot = take 1 xs
     pivotDeps = pivot >>= snd
     (sg3, sg4) = partition (\(_, xs') -> xs' == pivotDeps) (drop 1 xs)
     final = if null sg4 || null sg3 then xs else pivot ++ sg3 ++ foo sg4

chunks :: Set String -> [Exp] -> [[(String, [String])]]
chunks globals es = groupBy (\x y -> snd x == snd y) final
    where (g, f, _) = (G.graphFromEdges . ((\(x, y) -> (x, x, y)) <$>) . deps globals) es
          sg = reverse $ (\(_, y, z) -> (y, z)) . f <$> G.topSort g
          (sg1, sg2) = partition (\(_, xs) -> null xs) sg
          final = sg1 ++ foo sg2