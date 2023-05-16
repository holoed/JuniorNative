module Junior.Compiler.DependencyAnalysis where

import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Utils.Annotations ( Ann(Ann) )
import Junior.Core.Ast (Exp, ExpF (Defn, VarPat))
import Data.Set (Set, toList, empty)
import Data.List (groupBy, partition)
import Junior.Compiler.FreeVariables (freeVars)
import qualified Data.Graph as G

getName :: Exp -> Maybe String
getName (In (Ann _ (Defn _ (In (Ann _ (VarPat s))) _))) = Just s
getName _ = Nothing

getDeps :: Set String -> Exp -> Maybe (String, [String])
getDeps globals e = case getName e of
    Just name -> Just (name, toList (snd xs))
    Nothing -> Nothing
  where
    xs = case freeVars globals e of 
        (In (Ann xs' (Defn {}))) -> xs'
        _ -> (Nothing, empty)          

deps :: Set String -> [Exp] -> [(String, [String])]
deps globals = foldl (\acc x -> maybe acc (:acc) (getDeps globals x)) []

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