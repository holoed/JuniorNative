module OptimizeTypeClasses where

import TypedAst (TypedExp, TypedExpF)
import Fixpoint (Fix(In))
import Control.Monad.Trans.Reader ( ask, local, ReaderT(runReaderT) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import RecursionSchemes (cataRec)
import Annotations (Ann(Ann))
import Ast (ExpF(VarPat, AppClosure, Var, Let, MkTuple, App))
import Data.Map ( insert, empty, member, Map, (!), delete )
import Debug.Trace (trace)
import Prelude hiding (lookup)
import Control.Monad ((>=>))

type OptimizeM = ReaderT (Map String TypedExp) (State (Map String TypedExp))

optimizeImp :: [TypedExp] -> OptimizeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (OptimizeM TypedExp) -> OptimizeM TypedExp
        alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            case (n', v') of
             (In (Ann _ (VarPat k)) ,In (Ann _ (Var "nativeEqInt"))) -> do
                 state <- get
                 put (insert k v' state)
                 b' <- local (insert k v') b
                 return $ trace (k ++ " to be inlined") $ In (Ann attr (Let n' v' b'))
             (In (Ann _ (VarPat k)) , In (Ann _ (AppClosure (In (Ann _ (Var "nativeEqInt"))) _))) -> do
                 state <- get
                 put (insert k v' state)
                 b' <- local (insert k v') b
                 return $ trace (k ++ " to be inlined") $ In (Ann attr (Let n' v' b'))
             (In (Ann _ (VarPat k)) , _) -> do
                 b' <- local (insert k v') b
                 return $ trace (k ++ " in context") $ In (Ann attr (Let n' v' b'))
             (_, _) -> In . Ann attr . Let n' v' <$> b
        alg (Ann attr (AppClosure e1 e2)) = do
            e1' <- e1
            e2' <- e2
            case (e1', e2') of
                (In (Ann _ (AppClosure l@(In (Ann _ (Var "nativeEqInt"))) e3)), _) ->
                    return (In (Ann attr (App l (In (Ann attr (MkTuple [e3,e2']))))))
                (In (Ann _ (Var "==")), In (Ann _ (Var "eqInt"))) ->
                    return $ trace "Optimized __eqeq" $ In (Ann attr (Var "nativeEqInt"))
                (In (Ann _ (Var n1)), _) -> do
                    state <- get
                    if member n1 state
                    then do
                        put $ delete n1 state
                        return $ trace ("inlined " ++ n1) $ In (Ann attr (AppClosure (state!n1) e2'))
                    else return $ In (Ann attr (AppClosure e1' e2'))
                (_, _) ->
                    return $ In (Ann attr (AppClosure e1' e2'))
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es = evalState (runReaderT ((optimizeImp >=> optimizeImp) es) empty) empty