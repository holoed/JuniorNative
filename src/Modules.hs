module Modules where

import Ast ( Exp, ExpF(Let, VarPat) )
import Fixpoint ( Fix(In) )
import Infer ( infer )
import Environment ( Env, toEnv, concatEnvs )
import Data.Map as Map ( keysSet, fromList, toList, (!), restrictKeys, Map )
import Data.Set as Set (fromList)
import Data.List (sortOn)
import Types ( Qual((:=>)), Type(TyCon), TypeScheme(ForAll) ) 
import SynExpToExp ( toExp )
import Parser ( parseExpr )
import DagBindings ( chunks )
import LiftNumbers ( liftN )
import Annotations (Ann(..))
import ContextReduction (ClassEnv)
import Control.Parallel.Strategies (parMap, rdeepseq, NFData)

bindingsDict :: [Exp] -> Map String Exp 
bindingsDict es = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)

foo :: [[(String, Exp)]] -> ClassEnv -> Env -> Env
foo bss classEnv env = 
      foldl (\acc bs -> foldl concatEnvs acc $ parMap rdeepseq (f acc) bs) env bss
   where
     f env' (n, e) = 
       let t = (either (\err -> Set.fromList [] :=> TyCon err) snd . infer classEnv env' . liftN) e in
       toEnv [(n, t)] 

typeOfModule :: ClassEnv -> Env -> String -> [(String, String)] 
typeOfModule classEnv env x = sortedRet
    where es = either error (toExp <$>) (parseExpr x)
          ns = (fst <$>) <$> chunks (Map.keysSet env) es
          dict = bindingsDict es
          bs = ((\n -> (n, dict!n)) <$>) <$> ns
          flattenedNs = concat ns
          finalEnv = restrictKeys (foo bs classEnv env) (Set.fromList flattenedNs)
          unsortedRet = (\(n, ForAll _ qt) -> (n, show qt)) <$> Map.toList finalEnv
          orderDict = Map.fromList (zip flattenedNs ([1..] :: [Int]))
          sortedRet = sortOn ((orderDict!) . fst) unsortedRet