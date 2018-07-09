module Infer where

import Data.Map (empty)
import Data.Set (fromList, insert)
import Monads
import RecursionSchemes
import Ast
import Types
import BuiltIns
import Environment
import Substitutions
import InferMonad
import Unification
import PrettyTypes

valueToType :: Prim -> Type
valueToType (I _) = intCon
valueToType (B _) = boolCon
valueToType _ = undefined

alg :: ExpF (TypeM Exp) -> TypeM Exp
alg (Lit v) =
  do bt <- getBaseType
     updateSubs $ mgu (valueToType v) bt
     return (lit v)

alg (Var n) =
  do bt <- getBaseType
     t <- getTypeForName n
     updateSubs $ mgu t bt
     return (var n)

alg (App e1 e2) =
  do t1 <- newTyVar
     e1' <- local (\(env, t, sv) -> (env, TyLam t1 t, sv)) e1
     e2' <- local (\(env, _, sv)  -> (env, t1, sv)) e2
     return (app e1' e2')

alg (Lam n e) =
  do bt <- getBaseType
     t1@(TyVar t1n) <- newTyVar
     t2 <- newTyVar
     let t = TyLam t1 t2
     updateSubs $ mgu t bt
     e' <- local (\(env, _, sv) -> (addScheme n (Identity t1) env, t2, insert t1n sv)) e
     return (lam n e')

alg (IfThenElse p e1 e2) =
  do p' <- local (\(env, _, sv) -> (env, boolCon, sv)) p
     e1' <- e1
     (subs, _) <- get
     e2' <- local (\(env, t, sv) -> (env, substitute subs t, sv)) e2
     return (ifThenElse p' e1' e2')

alg (Let n e1 e2) =
  do t@(TyVar tn) <- newTyVar
     e1' <- local (\(env, _, sv) -> (addScheme n (Identity t) env, t, insert tn sv)) e1
     (subs, _) <- get
     e2' <- local (\(env, bt, sv) -> (addScheme n (generalise sv (substitute subs t)) env, bt, sv)) e2
     return (leT n e1' e2')

alg (MkTuple es) =
  do bt <- getBaseType
     ts <- mapM (const newTyVar) es
     let t = tupleCon ts
     updateSubs $ mgu t bt
     es' <- sequence (fmap (\(e, t') -> local (\(env, _, sv) -> (env, t', sv)) e) (zip es ts))
     return (mkTuple es')

infer :: Env -> Exp -> Either String Type
infer env e = fmap f (run m ctx state)
  where
        f (subs, _) = pretty (substitute subs bt)
        m = cataRec alg e
        bt =  TyVar "TBase"
        ctx = (env, bt, fromList [])
        state = (empty, 0)
