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

extract :: (String, Int) -> String -> Map.Map String ClosedExp -> ClosedExp
extract (x, i) env ctx =
  fromMaybe (In (Inr $ LookupEnv env i)) (Map.lookup x ctx)

newCtx :: Set String -> String -> Map.Map String ClosedExp -> Map.Map String ClosedExp
newCtx fv env ctx  =
  Map.fromList [(x, extract (x, i) env ctx) | (x, i) <- toList fv `zip` [0..]]

newEnv :: Set String -> Map.Map String ClosedExp -> [ClosedExp]
newEnv fv ctx =
  fmap (\x -> fromMaybe (In $ Inl $ Var x) (Map.lookup x ctx)) (toList fv)

alg :: Ann (Set String) ExpF (CloseM ClosedExp) -> CloseM ClosedExp
alg (Ann fv (Lam n e)) = do
     env <- gensym "env"
     ctx <- ask
     e' <- local (newCtx fv env) e
     return $ (mkClosure (mkEnv env (newEnv fv ctx)) . cLam n) e'
alg (Ann _ (Var s)) = do
     ctx <- ask
     let v = fromMaybe (In (Inl (Var s))) (Map.lookup s ctx)
     return v
alg (Ann _ x) = fmap (In . Inl) (traverse id x)

convert :: Exp -> Either String ClosedExp
convert e =
  let e' = freeVars e in
  eval (cataRec alg e') (Map.fromList []) 0
