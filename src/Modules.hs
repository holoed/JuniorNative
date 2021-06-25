module Modules where

import Ast ( Exp, ExpF(Let, VarPat) )
import Fixpoint ( Fix(In) )
import Infer ( infer )
import Environment ( Env, toEnv )
import Data.Map as Map ( keysSet, fromList, toList, (!), union, restrictKeys, Map )
import Data.Set as Set (fromList)
import Data.List (sortOn)
import Types ( Qual((:=>)), Type(TyCon), TypeScheme(ForAll), Pred ) 
import SynExpToExp ( toExp )
import Parser ( parseExpr )
import DagBindings ( chunks )
import LiftNumbers ( liftN )
import Annotations (Ann(..))

bindingsDict :: [Exp] -> Map String Exp 
bindingsDict es = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)

typeOfModule :: [Qual Pred] -> Env -> String -> [(String, String)] 
typeOfModule classEnv env x = sortedRet
    where es = either error (toExp <$>) (parseExpr x)
          ns = (fst <$>) $ concat $ chunks (Map.keysSet env) es
          dict = bindingsDict es
          bs = (\n -> (n, dict!n)) <$> ns
          f env' (n, e) = 
             let t = (either (\err -> Set.fromList [] :=> TyCon err) snd . infer classEnv env' . liftN) e in
             toEnv [(n, t)] `union` env'    
          finalEnv = restrictKeys (foldl f env bs) (Set.fromList ns)
          unsortedRet = (\(n, ForAll _ qt) -> (n, show qt)) <$> Map.toList finalEnv
          orderDict = Map.fromList (zip ns ([1..] :: [Int]))
          sortedRet = sortOn ((orderDict!) . fst) unsortedRet