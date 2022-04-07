module OptimizeTypeClasses where

import TypedAst (TypedExp, TypedExpF)
import Fixpoint (Fix(In))
import Control.Monad.State.Lazy (evalState, MonadState (put, get), State)
import RecursionSchemes (cataRec)
import Annotations (Ann(Ann))
import Ast (ExpF(VarPat, AppClosure, Var, Let))
import Data.Map ( insert, empty, member, Map, (!) )
import Debug.Trace (trace)
import Prelude hiding (lookup)
import Control.Monad ((>=>))

type OptimizeM = State (Map String TypedExp)

optimizeImp :: [TypedExp] -> OptimizeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (OptimizeM TypedExp) -> OptimizeM TypedExp
        alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            case (n', v') of
             (In (Ann _ (VarPat k)) ,In (Ann attr2 (Var "=="))) -> do
                 state <- get
                 put (insert k (In (Ann attr2 (Var "nativeEq"))) state)
                 b' <- b
                 return $ trace "match" $ In (Ann attr (Let n' v' b'))
             (_, _) -> In . Ann attr . Let n' v' <$> b
        alg (Ann attr (Var n)) = do
            state <- get
            if member n state
            then return $ trace "replaced" $ state!n
            else return $ In (Ann attr (Var n))
        alg (Ann attr (AppClosure e1 e2)) = do
            e1' <- e1
            e2' <- e2
            case (e1', e2') of
                (In (Ann _ (Var "==")), In (Ann _ (Var "eqInt"))) ->
                    return $ trace "Optimized __eqeq" $ In (Ann attr (Var "=="))
                (_, _) ->
                    return $ In (Ann attr (AppClosure e1' e2'))
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es = evalState ((optimizeImp >=> optimizeImp) es) empty