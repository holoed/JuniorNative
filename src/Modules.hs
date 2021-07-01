module Modules where

import Location ( PString )
import Ast ( Exp, ExpF(Let, VarPat) )
import Fixpoint ( Fix(In) )
import Infer ( infer )
import Environment ( Env, toEnv, concatEnvs )
import Data.Map as Map ( keysSet, fromList, toList, (!), restrictKeys, Map )
import Data.Set as Set (fromList)
import Data.List (sortOn)
import Types ( TypeScheme(ForAll) )
import SynExpToExp ( toExp )
import Parser ( parseExpr )
import DagBindings ( chunks )
import LiftNumbers ( liftN )
import Annotations (Ann(..))
import ContextReduction (ClassEnv)
import Control.Parallel.Strategies (parMap, rdeepseq)

bindingsDict :: [Exp] -> Map String Exp
bindingsDict es = Map.fromList ((\e@(In (Ann _ (Let (In (Ann _ (VarPat s))) _ _))) -> (s, e)) <$> es)

inferTypesFromDaggedBindings :: [[(String, Exp)]] -> ClassEnv -> Either PString Env -> Either PString Env
inferTypesFromDaggedBindings bss classEnv env =
      foldl (\acc bs -> foldl (\ev1 ev2 -> concatEnvs <$> ev1 <*> ev2) acc $ parMap rdeepseq (f acc) bs) env bss
   where
     g env' (n, e) = (\t -> toEnv [(n, t)]) . snd <$> (infer classEnv env' . liftN) e
     f env' (n, e) = env' >>= flip g (n, e)

inferTypes :: [Exp] -> ClassEnv -> Env -> Either PString [(String, String)]
inferTypes es classEnv env  = sortOn ((orderDict!) . fst) <$> unsortedRet
  where
          ns = (fst <$>) <$> chunks (Map.keysSet env) es
          dict = bindingsDict es
          bs = ((\n -> (n, dict!n)) <$>) <$> ns
          flattenedNs = concat ns
          env' = inferTypesFromDaggedBindings bs classEnv (Right env)
          finalEnv = flip restrictKeys (Set.fromList flattenedNs) <$> env'
          unsortedRet = fmap (\ (n, ForAll _ qt) -> (n, show qt)) . toList <$> finalEnv
          orderDict = Map.fromList (zip flattenedNs ([1..] :: [Int]))


typeOfModule :: ClassEnv -> Env -> String -> Either PString [(String, String)]
typeOfModule classEnv env x =
    parseExpr x >>= (\es' -> inferTypes es' classEnv env) . (toExp <$>)
  