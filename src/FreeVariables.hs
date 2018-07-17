module FreeVariables where

import Ast
import Fixpoint
import Annotations
import RecursionSchemes
import qualified Data.Set as Set
import Control.Monad.State

type FreeVarsM = State (Set.Set String)
type FreeVarsExp = Fix (Ann (Set.Set String) ExpF)

alg :: ExpF (FreeVarsM (Fix (Ann (Set.Set String) ExpF))) -> FreeVarsM (Fix (Ann (Set.Set String) ExpF))
alg (Lit x) = return $ In (Ann (Set.fromList []) (Lit x))
alg (Var s) = do modify (Set.insert s)
                 fvs <- get
                 return $ In (Ann fvs (Var s))
alg (MkTuple xs) = do xs' <- sequence xs
                      fvs <- get
                      return $ In (Ann fvs $ MkTuple xs')
alg (App e1 e2) = do e1' <- e1
                     e2' <- e2
                     fvs <- get
                     return $ In (Ann fvs $ App e1' e2')
alg (Lam s e) = do e' <- e
                   modify (Set.delete s)
                   fvs <- get
                   return $ In (Ann fvs $ Lam s e')
alg (Let s v b) = do v' <- v
                     b' <- b
                     modify (Set.delete s)
                     fvs <- get
                     return $ In (Ann fvs $ Let s v' b')
alg (IfThenElse p e1 e2) = do p' <- p
                              e1' <- e1
                              e2' <- e2
                              fvs <- get
                              return $ In (Ann fvs $ IfThenElse p' e1' e2')


freeVars :: Exp -> Fix (Ann (Set.Set String) ExpF)
freeVars e = evalState (cataRec alg e) (Set.fromList [])
