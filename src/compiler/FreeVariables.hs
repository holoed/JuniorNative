module FreeVariables where

import Ast ( ExpF(..) )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(..), unwrap )
import RecursionSchemes ( cataRec )
import Data.Set ( delete, empty, singleton, union, member, Set )
import Control.Monad.Writer ( runWriter, MonadWriter(listen, tell, pass), Writer )

type FreeVarsM = Writer (Set String)
type FreeVarsExp attr = Fix (Ann (attr, Set String) ExpF)

getNames :: FreeVarsExp attr -> [String]
getNames (In (Ann (_, _) (VarPat s))) = [s]
getNames (In (Ann (_, _) (TuplePat ss))) = ss >>= getNames
getNames x = error $ "getNames: Unexpected exp " ++ show (unwrap x)

freeVars :: Set String -> Fix (Ann attr ExpF) -> Fix (Ann (attr, Set String) ExpF)
freeVars globals e = fst $ runWriter (cataRec alg e)
  where
    alg :: Ann attr ExpF (FreeVarsM (FreeVarsExp attr)) -> FreeVarsM (FreeVarsExp attr)
    alg (Ann l (Lit x)) = return $ In (Ann (l, empty) (Lit x))
    alg (Ann l (Var s)) = do
      let fv = if member s globals
               then empty
               else singleton s
      tell fv
      return $ In (Ann (l, fv) (Var s))
    alg (Ann l (VarPat s)) = do
      let fv = if member s globals
               then empty
               else singleton s
      tell fv
      return $ In (Ann (l, fv) (VarPat s))
    alg (Ann l (MkTuple xs)) = do
      (xs', fvs) <- listen (sequence xs)
      return $ In (Ann (l, fvs) $ MkTuple xs')
    alg (Ann l (TuplePat xs)) = do
      (xs', fvs) <- listen (sequence xs)
      return $ In (Ann (l, fvs) $ TuplePat xs')
    alg (Ann l (App e1 e2)) = do
      (e1', fvs1) <- listen e1
      (e2', fvs2) <- listen e2
      return $ In (Ann (l, fvs1 `union` fvs2) $ App e1' e2')
    alg (Ann l (Lam n e')) = pass (do
      n' <- n
      let s' = getNames n'
      (e'', fvs) <- listen e'
      let f x = foldl (flip delete) x s'
      return (In (Ann (l, f fvs) $ Lam n' e''), f))
    alg (Ann l (Let n v b)) = pass (do 
       n' <- n
       let s' = getNames n'
       (v', fvs1) <- listen v
       (b', fvs2) <- listen b
       let f x = foldl (flip delete) x s'
       return (In (Ann (l, f $ fvs1 `union` fvs2) $ Let n' v' b'), f))
    alg (Ann l (IfThenElse p e1 e2)) = do
      (p', fvs1) <- listen p
      (e1',fvs2) <- listen e1
      (e2',fvs3) <- listen e2
      return $ In (Ann (l, fvs1 `union` fvs2 `union` fvs3) $ IfThenElse p' e1' e2')
    alg (Ann l (Defn qt n v)) = pass (do 
       n' <- n
       let s' = getNames n'
       (v', fvs1) <- listen v
       let f x = foldl (flip delete) x s'
       return (In (Ann (l, f fvs1) $ Defn qt n' v'), f))
    {- Dedicated to Closure Conversion -}
    alg (Ann l (MkClosure n)) = return $ In (Ann (l, empty) (MkClosure n))
    alg (Ann l (SetEnv n e1 e2)) = do
      (e1',fvs1) <- listen e1
      (e2',fvs2) <- listen e2
      return $ In (Ann (l, fvs1 `union` fvs2) $ SetEnv n e1' e2')
    alg (Ann l (GetEnv n e1)) = do
      (e1', fvs) <- listen e1 
      return $ In (Ann (l, fvs) (GetEnv n e1'))
    alg (Ann l (ClosureRef e1)) = do 
      (e1', fvs) <- listen e1 
      return $ In (Ann (l, fvs) (ClosureRef e1'))

    



