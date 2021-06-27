module Infer where

import Data.Map (empty)
import Data.Set (fromList, insert, union)
import Monads ( local, get, listen, run, throwError )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import RecursionSchemes ( cataRec )
import Primitives ( Prim(..) )
import Ast ( Exp, ExpF(..), Loc )
import TypedAst ( TypedExp, tlit, tvar, tapp, tlam, tleT, tifThenElse, tmkTuple, tvarPat, ttuplePat )
import Types ( TypeScheme(Identity), Type(..), Qual(..), Pred, clean, deleteTautology )
import BuiltIns ( boolCon, intCon, doubleCon, strCon, tupleCon )
import Environment ( Env, addScheme )
import Substitutions ( Substitutions, substitute, substituteQ )
import InferMonad ( TypeM, newTyVar, getBaseType, getTypeForName, generalise, substituteQM )
import Unification ( mgu )
import PrettyTypes ( prettyQ )
import ContextReduction (resolvePreds, ClassEnv)

getNameAndTypes :: TypedExp -> [(String, Qual Type)]
getNameAndTypes (In (Ann (_, qt) (VarPat s))) = [(s, qt)]
getNameAndTypes (In (Ann (_, _) (TuplePat xs))) = xs >>= getNameAndTypes

valueToType :: Prim -> Type
valueToType (I _) = intCon
valueToType (D _) = doubleCon
valueToType (B _) = boolCon
valueToType (S _) = strCon
valueToType U     = TyCon "()"

foldToScheme :: Env ->  [(String, Qual Type)] -> Env
foldToScheme = foldl (\env' (n, qt) -> addScheme n (Identity qt) env')

alg :: Ann (Maybe Loc) ExpF (TypeM TypedExp) -> TypeM TypedExp
alg (Ann (Just l) (Lit v)) =
  do bt <- getBaseType
     mgu l (valueToType v) bt
     return (tlit l (fromList [] :=> bt) v)

alg (Ann (Just l) (Var n)) =
  do bt <- getBaseType
     (t, ps) <- listen (getTypeForName n)
     mgu l t bt
     return (tvar l (ps :=> bt) n)

alg (Ann Nothing  (App e1 e2)) =
  do t1 <- newTyVar 0
     (e1', ps1) <- listen $ local (\(env, t, sv) -> (env, TyApp (TyApp (TyCon "->") t1) t, sv)) e1
     (e2', ps2) <- listen $ local (\(env, _, sv)  -> (env, t1, sv)) e2
     bt <- getBaseType
     qt <- substituteQM ((ps1 `union` ps2) :=> bt)
     return (tapp qt e1' e2')

alg (Ann (Just l) (Lam n e)) =
  do n'@(In (Ann (_, _ :=> t0) _)) <- n
     let nts = getNameAndTypes n'
     bt <- getBaseType
     t2 <- newTyVar 0
     let t = TyApp (TyApp (TyCon "->") t0) t2
     mgu l t bt
     let (TyVar t1n _) = t0
     (e', ps'') <- listen $ local (\(env, _, sv) ->
       (foldToScheme env nts, t2, insert t1n sv)) e
     return (tlam l (ps'' :=> t) n' e')

alg (Ann (Just l) (IfThenElse p e1 e2)) =
  do (p', ps1) <- listen $ local (\(env, _, sv) -> (env, boolCon, sv)) p
     (e1', ps2) <- listen e1
     (subs, _) <- get
     (e2', ps3) <- listen $ local (\(env, t, sv) -> (env, substitute subs t, sv)) e2
     (subs', _) <- get
     bt <- getBaseType
     let qt = substituteQ subs' ((ps1 `union` ps2 `union` ps3) :=> bt)
     return (tifThenElse l qt p' e1' e2')

alg (Ann (Just l) (Let n e1 e2)) =
  do n'@(In (Ann (_, _ :=> t0) _)) <- n
     let nts = getNameAndTypes n'
     let (TyVar tn _) = t0
     (e1', ps1) <- listen $ local (\(env, _, sv) -> 
       (foldToScheme env nts, t0, insert tn sv)) e1
     (subs, _) <- get
     (e2', ps2) <- listen $ local (\(env, bt, sv) -> 
       (foldl (\env' (n, ps2 :=> t3) -> addScheme n (generalise sv (substituteQ subs ((ps1 `union` ps2) :=> t3))) env') env nts, bt, sv)) e2
     bt <- getBaseType
     return (tleT l ((ps1 `union` ps2) :=> bt) n' e1' e2')

alg (Ann (Just l) (MkTuple es)) =
  do bt <- getBaseType
     ts <- mapM (const (newTyVar 0)) es
     let t = tupleCon ts
     mgu l t bt
     (es', ps) <- listen $ traverse (\(e, t') -> local (\(env, _, sv) -> (env, t', sv)) e) (zip es ts)
     return (tmkTuple l (ps :=> t) es')

alg (Ann (Just l) (VarPat s)) = do
  t <- newTyVar 0
  return $ tvarPat l (fromList [] :=> t) s

alg (Ann (Just l) (TuplePat ns)) = do
  ns' <- sequence ns
  let nts = ns' >>= getNameAndTypes
  let t = tupleCon ((\(_, _ :=> t) -> t) <$> nts)
  return $ ttuplePat l (fromList [] :=> t) ns'

alg _ = throwError "Undefined"

infer :: ClassEnv -> Env -> Exp -> Either String (Substitutions, Qual Type)
infer classEnv env e = fmap f (run (m >>= resolvePreds classEnv) ctx state)
  where
        f (In(Ann (_, qt) _), (subs, _), _) =  (subs, (prettyQ . deleteTautology . clean . substituteQ subs) qt)
        m = cataRec alg e
        bt =  TyVar "TBase" 0
        ctx = (env, bt, fromList [])
        state = (empty, 0)
 