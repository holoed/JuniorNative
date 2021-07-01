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
import Control.Parallel.Strategies (parMap, rdeepseq)

bindingsDict :: [Exp] -> Map String Exp
bindingsDict es = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)

inferTypesFromDaggedBindings :: [[(String, Exp)]] -> ClassEnv -> Env -> Env
inferTypesFromDaggedBindings bss classEnv env =
      foldl (\acc bs -> foldl concatEnvs acc $ parMap rdeepseq (f acc) bs) env bss
   where
     f env' (n, e) =
       let t = (either (\err -> Set.fromList [] :=> TyCon err) snd . infer classEnv env' . liftN) e in
       toEnv [(n, t)]


inferTypes :: [Exp] -> ClassEnv -> Env -> [(String, String)]
inferTypes es classEnv env  = sortOn ((orderDict!) . fst) unsortedRet
  where
          ns = (fst <$>) <$> chunks (Map.keysSet env) es
          dict = bindingsDict es
          bs = ((\n -> (n, dict!n)) <$>) <$> ns
          flattenedNs = concat ns
          finalEnv = restrictKeys (inferTypesFromDaggedBindings bs classEnv env) (Set.fromList flattenedNs)
          unsortedRet = (\(n, ForAll _ qt) -> (n, show qt)) <$> Map.toList finalEnv
          orderDict = Map.fromList (zip flattenedNs ([1..] :: [Int]))


typeOfModule :: ClassEnv -> Env -> String -> Either String [(String, String)]
typeOfModule classEnv env x =
    (\es' -> inferTypes es' classEnv env) <$> ((toExp <$>) <$> parseExpr x)
  