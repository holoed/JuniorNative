module Infer where

import Data.Map (empty)
import Data.Set (fromList, insert, union)
import Annotations
import Monads
import RecursionSchemes
import Fixpoint
import Ast
import TypedAst
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
valueToType (S _) = strCon


alg :: ExpF (TypeM TypedExp) -> TypeM TypedExp
alg (Lit v) =
  do bt <- getBaseType
     mgu (valueToType v) bt
     return (tlit (fromList [] :=> bt) v)

alg (Var n) =
  do bt <- getBaseType
     (t, ps) <- listen (getTypeForName n)
     mgu t bt
     return (tvar (ps :=> bt) n)

alg (App e1 e2) =
  do t1 <- newTyVar
     (e1', ps1) <- listen $ local (\(env, t, sv) -> (env, TyLam t1 t, sv)) e1
     (e2', ps2) <- listen $ local (\(env, _, sv)  -> (env, t1, sv)) e2
     bt <- getBaseType
     qt <- substituteQM ((ps1 `union` ps2) :=> bt)
     return (tapp qt e1' e2')

alg (Lam n e) =
  do bt <- getBaseType
     t1 <- newTyVar
     t2 <- newTyVar
     let t = TyLam t1 t2
     mgu t bt
     let (TyVar t1n _) = t1
     (e', ps) <- listen $ local (\(env, _, sv) -> (addScheme n (Identity (fromList [] :=> t1)) env, t2, insert t1n sv)) e
     return (tlam (ps :=> t) n e')

alg (IfThenElse p e1 e2) =
  do (p', ps1) <- listen $ local (\(env, _, sv) -> (env, boolCon, sv)) p
     (e1', ps2) <- listen e1
     (subs, _) <- get
     (e2', ps3) <- listen $ local (\(env, t, sv) -> (env, substitute subs t, sv)) e2
     bt <- getBaseType
     return (tifThenElse ((ps1 `union` ps2 `union` ps3) :=> bt) p' e1' e2')

alg (Let n e1 e2) =
  do t <- newTyVar
     let (TyVar tn _) = t
     (e1', ps1) <- listen $ local (\(env, _, sv) -> (addScheme n (Identity (fromList [] :=> t)) env, t, insert tn sv)) e1
     (subs, _) <- get
     (e2', ps2) <- listen $ local (\(env, bt, sv) -> (addScheme n (generalise sv (substituteQ subs (ps1 :=> t))) env, bt, sv)) e2
     bt <- getBaseType
     return (tleT ((ps1 `union` ps2) :=> bt) n e1' e2')

alg (MkTuple es) =
  do bt <- getBaseType
     ts <- mapM (const newTyVar) es
     let t = tupleCon ts
     mgu t bt
     (es', ps) <- listen $ traverse (\(e, t') -> local (\(env, _, sv) -> (env, t', sv)) e) (zip es ts)
     return (tmkTuple (ps :=> t) es')

infer :: Env -> Exp -> Either String (Substitutions, Qual Type)
infer env e = fmap f (run m ctx state)
  where
        f ((subs, _), ps) =  (subs, (prettyQ . deleteTautology . clean . (substituteQ subs)) (ps :=> bt))
        m = cataRec alg (desugarOps e)
        bt =  TyVar "TBase" 0
        ctx = (env, bt, fromList [])
        state = (empty, 0)


desugarOps :: Exp -> Exp
desugarOps = cataRec alg
    where alg (InfixApp (txt, _, _) e1 e2) | txt /= " " = app (app (var txt) e1) e2
          alg (InfixApp (txt, _, _) e1 e2) = app e1 e2
          alg e = In e  