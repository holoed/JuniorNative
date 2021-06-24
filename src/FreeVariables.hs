module FreeVariables where

import Ast ( Exp, ExpF(..), Loc )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(..) )
import RecursionSchemes ( cataRec )
import Data.Set ( delete, empty, singleton, union, member, Set )
import Control.Monad.Writer ( runWriter, MonadWriter(listen, tell, pass), Writer )

type FreeVarsM = Writer (Set String)
type FreeVarsExp = Fix (Ann (Maybe Loc, Set String) ExpF)

getName :: FreeVarsExp -> String 
getName (In (Ann (_, _) (VarPat s))) = s

freeVars :: Set String -> Exp -> Fix (Ann (Maybe Loc, Set String) ExpF)
freeVars globals e = fst $ runWriter (cataRec alg e)
  where
    alg :: Ann (Maybe Loc) ExpF (FreeVarsM FreeVarsExp) -> FreeVarsM FreeVarsExp
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
    alg (Ann l (App e1 e2)) = do 
      (e1', fvs1) <- listen e1
      (e2', fvs2) <- listen e2
      return $ In (Ann (l, fvs1 `union` fvs2) $ App e1' e2')
    alg (Ann l (Lam ps e')) = pass (do 
      ps' <- sequence ps
      let s' = head $ getName <$> ps'
      (e'', fvs) <- listen e'
      return (In (Ann (l, delete s' fvs) $ Lam ps' e''), delete s'))
    alg (Ann l (Let ps v b)) = pass (do 
       ps' <- sequence ps
       let s' = head $ getName <$> ps'
       (v', fvs1) <- listen v
       (b', fvs2) <- listen b
       return (In (Ann (l, delete s' $ fvs1 `union` fvs2) $ Let ps' v' b'), delete s'))
    alg (Ann l (IfThenElse p e1 e2)) = do 
      (p', fvs1) <- listen p
      (e1',fvs2) <- listen e1
      (e2',fvs3) <- listen e2
      return $ In (Ann (l, fvs1 `union` fvs2 `union` fvs3) $ IfThenElse p' e1' e2')



