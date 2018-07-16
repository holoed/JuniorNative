module FreeVariables where

import Ast
import Fixpoint
import Annotations
import RecursionSchemes
import qualified Data.Set as Set
import Control.Monad.State

alg :: ExpF (Set.Set String) -> Set.Set String
alg (Lit _) = Set.empty
alg (Var s) = Set.singleton s
alg (MkTuple xs) = Set.unions xs
alg (App e1 e2) = Set.union e1 e2
alg (Lam s e) = Set.delete s e
alg (Let s e1 e2) = Set.delete s (e1 `Set.union` e2)

freeVars :: Exp -> Set.Set String
freeVars = cataRec alg

type FreeVarsM = State (Set.Set String)
type FreeVarsExp = Fix (Ann (Set.Set String) ExpF)

alg2 :: ExpF (FreeVarsM (Fix (Ann (Set.Set String) ExpF))) -> FreeVarsM (Fix (Ann (Set.Set String) ExpF))
alg2 (Lit x) = return $ In (Ann (Set.fromList []) (Lit x))
alg2 (Var s) = do modify (Set.insert s)
                  fvs <- get
                  return $ In (Ann fvs (Var s))
alg2 (MkTuple xs) = do xs' <- sequence xs
                       fvs <- get
                       return $ In (Ann fvs $ MkTuple xs')
alg2 (App e1 e2) = do e1' <- e1
                      e2' <- e2
                      fvs <- get
                      return $ In (Ann fvs $ App e1' e2')
alg2 (Lam s e) = do e' <- e
                    modify (Set.delete s)
                    fvs <- get
                    return $ In (Ann fvs $ Lam s e')    

freeVarsExp :: Exp -> Fix (Ann (Set.Set String) ExpF)
freeVarsExp e = evalState (cataRec alg2 e) (Set.fromList [])
