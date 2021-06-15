module FreeVariables where

import Ast ( Exp, ExpF(..) )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(..) )
import RecursionSchemes ( cataRec )
import Data.Set ( delete, empty, singleton, union, member, Set )
import Control.Monad.Writer ( runWriter, MonadWriter(listen, tell, pass), Writer )

type FreeVarsM = Writer (Set String)
type FreeVarsExp = Fix (Ann (Set String) ExpF)

freeVars :: Set String -> Exp -> Fix (Ann (Set String) ExpF)
freeVars globals e = fst $ runWriter (cataRec alg e)
  where
    alg :: ExpF (FreeVarsM FreeVarsExp) -> FreeVarsM FreeVarsExp
    alg (Lit x) = return $ In (Ann empty (Lit x))
    alg (Var s) = do let fv = if member s globals
                              then empty
                              else singleton s
                     tell fv
                     return $ In (Ann fv (Var s))
    alg (MkTuple xs) = do (xs', fvs) <- listen (sequence xs)
                          return $ In (Ann fvs $ MkTuple xs')
    alg (App e1 e2) = do (e1', fvs1) <- listen e1
                         (e2', fvs2) <- listen e2
                         return $ In (Ann (fvs1 `union` fvs2) $ App e1' e2')
    alg (Lam s e') = pass (do (e'', fvs) <- listen e'
                              return (In (Ann (delete s fvs) $ Lam s e''), delete s))
    alg (Let s v b) = pass (do (v', fvs1) <- listen v
                               (b', fvs2) <- listen b
                               return (In (Ann (delete s $ fvs1 `union` fvs2) $ Let s v' b'), delete s))
    alg (IfThenElse p e1 e2) = do (p', fvs1) <- listen p
                                  (e1',fvs2) <- listen e1
                                  (e2',fvs3) <- listen e2
                                  return $ In (Ann (fvs1 `union` fvs2 `union` fvs3) $ IfThenElse p' e1' e2')



