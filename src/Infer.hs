module Infer where

import Data.Map (empty)
import Data.Set (fromList, insert, union)
import Monads ( local, get, listen, run, throwError )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import RecursionSchemes ( cataRec )
import Primitives ( Prim(..) )
import Ast ( Exp, ExpF(..), ExpLoc(..) )
import TypedAst ( TypedExp, tlit, tvar, tapp, tlam, tleT, tifThenElse, tmkTuple )
import Types ( TypeScheme(Identity), Type(..), Qual(..), Pred, clean, deleteTautology )
import BuiltIns ( boolCon, intCon, strCon, tupleCon )
import Environment ( Env, addScheme )
import Substitutions ( Substitutions, substitute, substituteQ )
import InferMonad ( TypeM, newTyVar, getBaseType, getTypeForName, generalise, substituteQM )
import Unification ( mgu )
import PrettyTypes ( prettyQ )
import ContextReduction (resolvePreds)

valueToType :: Prim -> Type
valueToType (I _) = intCon
valueToType (B _) = boolCon
valueToType (S _) = strCon
valueToType U     = TyCon "()"

alg :: Ann ExpLoc ExpF (TypeM TypedExp) -> TypeM TypedExp
alg (Ann (LitLoc l) (Lit v)) =
  do bt <- getBaseType
     mgu l (valueToType v) bt
     return (tlit l (fromList [] :=> bt) v)

alg (Ann (VarLoc l) (Var n)) =
  do bt <- getBaseType
     (t, ps) <- listen (getTypeForName n)
     mgu l t bt
     return (tvar l (ps :=> bt) n)

alg (Ann AppLoc (App e1 e2)) =
  do t1 <- newTyVar 0
     (e1', ps1) <- listen $ local (\(env, t, sv) -> (env, TyApp (TyApp (TyCon "->") t1) t, sv)) e1
     (e2', ps2) <- listen $ local (\(env, _, sv)  -> (env, t1, sv)) e2
     bt <- getBaseType
     qt <- substituteQM ((ps1 `union` ps2) :=> bt)
     return (tapp qt e1' e2')

alg (Ann (LamLoc l l') (Lam n e)) =
  do bt <- getBaseType
     t1 <- newTyVar 0
     t2 <- newTyVar 0
     let t = TyApp (TyApp (TyCon "->") t1) t2
     mgu l t bt
     let (TyVar t1n _) = t1
     (e', ps) <- listen $ local (\(env, _, sv) -> (addScheme n (Identity (fromList [] :=> t1)) env, t2, insert t1n sv)) e
     return (tlam l (ps :=> t) (n, l') e')

alg (Ann (IfThenElseLoc l) (IfThenElse p e1 e2)) =
  do (p', ps1) <- listen $ local (\(env, _, sv) -> (env, boolCon, sv)) p
     (e1', ps2) <- listen e1
     (subs, _) <- get
     (e2', ps3) <- listen $ local (\(env, t, sv) -> (env, substitute subs t, sv)) e2
     (subs', _) <- get
     bt <- getBaseType
     let qt = substituteQ subs' ((ps1 `union` ps2 `union` ps3) :=> bt)
     return (tifThenElse l qt p' e1' e2')

alg (Ann (LetLoc l l') (Let n e1 e2)) =
  do t <- newTyVar 0
     let (TyVar tn _) = t
     (e1', ps1) <- listen $ local (\(env, _, sv) -> (addScheme n (Identity (fromList [] :=> t)) env, t, insert tn sv)) e1
     (subs, _) <- get
     (e2', ps2) <- listen $ local (\(env, bt, sv) -> (addScheme n (generalise sv (substituteQ subs (ps1 :=> t))) env, bt, sv)) e2
     bt <- getBaseType
     return (tleT l ((ps1 `union` ps2) :=> bt) (n, l') e1' e2')

alg (Ann (TupleLoc l) (MkTuple es)) =
  do bt <- getBaseType
     ts <- mapM (const (newTyVar 0)) es
     let t = tupleCon ts
     mgu l t bt
     (es', ps) <- listen $ traverse (\(e, t') -> local (\(env, _, sv) -> (env, t', sv)) e) (zip es ts)
     return (tmkTuple l (ps :=> t) es')

alg _ = throwError "Undefined"

infer :: [Qual Pred] -> Env -> Exp -> Either String (Substitutions, Qual Type)
infer classEnv env e = fmap f (run (m >>= resolvePreds classEnv) ctx state)
  where
        f (In(Ann (_, qt) _), (subs, _), _) =  (subs, (prettyQ . deleteTautology . clean . substituteQ subs) qt)
        m = cataRec alg e
        bt =  TyVar "TBase" 0
        ctx = (env, bt, fromList [])
        state = (empty, 0)
 