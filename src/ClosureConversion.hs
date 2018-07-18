module ClosureConversion where

import Ast
import ClosedAst
import CoProduct
import RecursionSchemes
import Monads
import Annotations
import FreeVariables (freeVars)
import Fixpoint
import Data.Set
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Subst = Map.Map String ClosedExp

type CloseM = ReaderState Subst Int

gensym :: String -> CloseM String
gensym prefix = do
  n <- get
  modify (+1)
  return $ "_" ++ prefix ++ show n

alg :: Ann (Set String) ExpF (CloseM ClosedExp) -> CloseM ClosedExp
alg (Ann fv (Lam n e)) = do
     env <- gensym "env"
     let s = [(x, In (Inr $ LookupEnv (In $ Inl $ Var env) i)) | (x, i) <- toList fv `zip` [0..]]
     let s' = fmap (In . Inl . Var) (toList fv)
     e' <- local (\_ -> Map.fromList s) e
     let newEnv = mkEnv s'
     return $ (mkClosure newEnv . cLam env n) e'
alg (Ann _ (Var s)) = do
     ctx <- ask
     let v = fromMaybe (In (Inl (Var s))) (Map.lookup s ctx)
     return v
alg (Ann _ x) = fmap (In . Inl) (traverse id x)

convert :: Exp -> Either String ClosedExp
convert e =
  let e' = freeVars e in
  eval (cataRec alg e') (Map.fromList []) 0
