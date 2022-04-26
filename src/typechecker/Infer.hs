module Infer where

import Data.Map (empty)
import Data.Set (fromList, insert, union)
import Monads ( local, get, listen, run, throwError )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann), unwrap, mapAnn )
import RecursionSchemes ( cataRec )
import Primitives ( Prim(..) )
import Location ( Loc, PString(..) )
import Ast ( Exp, ExpF(..) )
import TypedAst ( TypedExp, tlit, tvar, tapp, tlam, tleT, tifThenElse, tmkTuple, tvarPat, ttuplePat, tdefn )
import Types ( TypeScheme(Identity), Type(..), Qual(..), clean, deleteTautology )
import BuiltIns ( boolCon, intCon, doubleCon, strCon, charCon, tupleCon )
import Environment ( Env, addScheme )
import Substitutions ( Substitutions, substituteQ )
import InferMonad ( TypeM, newTyVar, getBaseType, getTypeForName, generalise, substituteQM )
import Unification ( mgu )
import ContextReduction (resolvePreds, ClassEnv)
import MonomorphicRestriction (applyRestriction)
import Data.Bifunctor (second)
import Control.Monad ((<=<))
import Data.Maybe (isJust)

getNameAndTypes :: TypedExp -> [(String, Qual Type)]
getNameAndTypes (In (Ann (_, qt) (VarPat s))) = [(s, qt)]
getNameAndTypes (In (Ann (_, _) (TuplePat xs))) = xs >>= getNameAndTypes
getNameAndTypes x = error $ "getNames: Unexpected exp " ++ show (unwrap x)

generateTypeForPattern :: TypedExp -> TypeM Type
generateTypeForPattern (In (Ann (_, _ :=> t) (VarPat _))) = return t
generateTypeForPattern (In (Ann (_, _) (TuplePat xs))) = do
    ts <- sequence (generateTypeForPattern <$> xs)
    return $ tupleCon ts
generateTypeForPattern _ = undefined 

valueToType :: Prim -> Type
valueToType (I _) = intCon
valueToType (D _) = doubleCon
valueToType (B _) = boolCon
valueToType (S _) = strCon
valueToType (C _) = charCon
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
     (t, ps) <- listen (getTypeForName l n)
     mgu l t bt
     return (tvar l (ps :=> bt) n)

alg (Ann Nothing  (App e1 e2)) =
  do t1 <- newTyVar 0
     (e1', ps1) <- listen $ local (\(env, t, sv, b) -> (env, TyApp (TyApp (TyCon "->") t1) t, sv, b)) e1
     (e2', ps2) <- listen $ local (\(env, _, sv, b)  -> (env, t1, sv, b)) e2
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
     (e', ps'') <- listen $ local (\(env, _, sv, _) ->
       (foldToScheme env nts, t2, insert t1n sv, False)) e
     return (tlam l (ps'' :=> t) n' e')

alg (Ann (Just l) (IfThenElse p e1 e2)) =
  do t2 <- newTyVar 0
     bt <- getBaseType
     (p', ps1) <- listen $ local (\(env, _, sv, b) -> (env, boolCon, sv, b)) p
     (e1', ps2) <- listen $ local (\(env, _, sv, b) -> (env, t2, sv, b)) e1
     (e2', ps3) <- listen $ local (\(env, _, sv, b) -> (env, t2, sv, b)) e2
     mgu l t2 bt
     qt <- substituteQM ((ps1 `union` ps2 `union` ps3) :=> bt)
     return (tifThenElse l qt p' e1' e2')

alg (Ann (Just l) (Let n e1 e2)) =
  do n'@(In (Ann (_, _ :=> t0) _)) <- n
     let nts = getNameAndTypes n'
     let (TyVar tn _) = t0
     (e1', ps1) <- listen $ local (\(env, _, sv, _) ->
       (foldToScheme env nts, t0, insert tn sv, False)) e1
     (subs, _) <- get
     (e2', ps2) <- listen $ local (\(env, bt, sv, b) ->
       (foldl (\env' (n'', ps2 :=> t3) -> addScheme n'' (generalise b sv (substituteQ subs ((ps1 `union` ps2) :=> t3))) env') env nts, bt, sv, b)) e2
     bt <- getBaseType
     return (tleT l ((ps1 `union` ps2) :=> bt) n' e1' e2')

alg (Ann (Just l) (MkTuple es)) =
  do bt <- getBaseType
     ts <- mapM (const (newTyVar 0)) es
     let t = tupleCon ts
     mgu l t bt
     (es', ps) <- listen $ traverse (\(e, t') -> local (\(env, _, sv, b) -> (env, t', sv, b)) e) (zip es ts)
     return (tmkTuple l (ps :=> t) es')

alg (Ann (Just l) (VarPat s)) = do
  t <- newTyVar 0
  return $ tvarPat l (fromList [] :=> t) s

alg (Ann (Just l) (TuplePat ns)) = do
  ns' <- sequence ns
  t <- sequence (generateTypeForPattern <$> ns')
  return $ ttuplePat l (fromList [] :=> tupleCon t) ns'

alg (Ann (Just l) (Defn givenQt n e1)) =
  do n'@(In (Ann (_, _ :=> t0) _)) <- n
     let nts = getNameAndTypes n'
     let (TyVar tn _) = t0
     (e1', ps1) <- listen $ local (\(env, _, sv, _) ->
       (foldToScheme env nts, t0, insert tn sv, False)) e1
     bt <- getBaseType
     ps2 <- if isJust givenQt
            then do
                 let Just (givenPreds :=> givenType) = givenQt
                 mgu l givenType t0
                 return givenPreds
            else return (fromList [])
     mgu l t0 bt
     return (tdefn l (ps1 `union` ps2 :=> bt) givenQt n' e1')

alg _ = throwError $ PStr ("Undefined", Nothing)

infer :: ClassEnv -> Env -> Exp -> Either PString (Substitutions, TypedExp)
infer classEnv env e = fmap f (run (m >>= (applyRestriction <=< resolvePreds classEnv)) ctx state)
  where
        f (e2, (subs, _), _) = (subs, g subs e2)
        g subs = mapAnn (second (deleteTautology . clean . substituteQ subs))
        m = cataRec alg e
        bt =  TyVar "TBase" 0
        ctx = (env, bt, fromList [], True)
        state = (empty, 0)
 