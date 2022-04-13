module OptimizeTypeClasses where

import TypedAst (TypedExp, TypedExpF)
import Fixpoint (Fix(In))
import Control.Monad.Trans.Reader ( local, ReaderT(runReaderT) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import RecursionSchemes (cataRec)
import Annotations (Ann(Ann))
import Ast (ExpF(VarPat, AppClosure, Var, Let, App, GetEnv))
import Data.Map ( insert, empty, member, Map, (!), delete )
-- import Debug.Trace (trace)
import Prelude hiding (lookup)
import Control.Monad ((>=>))
import qualified Data.Set as Set 

identifierList :: Set.Set String
identifierList = Set.fromList [
    "nativeEqInt", "nativeAddInt", "nativeSubInt", "nativeMulInt", "nativeDivInt", "nativeInt", 
    "nativeEqDouble", "nativeAddDouble", "nativeSubDouble", "nativeMulDouble", "nativeDivDouble", "nativeDouble"]

type OptimizeM = ReaderT (Map String TypedExp) (State (Map String TypedExp))

optimizeImp :: [TypedExp] -> OptimizeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (OptimizeM TypedExp) -> OptimizeM TypedExp
        alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            case (n', v') of
             (In (Ann _ (VarPat k)) ,In (Ann _ (Var x))) | Set.member x identifierList -> do
                 state <- get
                 put (insert k v' state)
                 b' <- local (insert k v') b
                 return $ In (Ann attr (Let n' v' b'))
             (In (Ann _ (VarPat k)) , In (Ann _ (AppClosure (In (Ann _ (Var x))) _))) | Set.member x identifierList -> do
                 state <- get
                 put (insert k v' state)
                 b' <- local (insert k v') b
                 return $ In (Ann attr (Let n' v' b'))
             (In (Ann _ (VarPat k)) , _) -> do
                 b' <- local (insert k v') b
                 return $ In (Ann attr (Let n' v' b'))
             (_, _) -> In . Ann attr . Let n' v' <$> b
        alg (Ann attr (GetEnv "eqInt" _)) = 
            return $ In (Ann attr (Var "eqInt"))
        alg (Ann attr (GetEnv "numInt" _)) = 
            return $ In (Ann attr (Var "numInt"))
        alg (Ann attr (GetEnv "fractionalInt" _)) = 
            return $ In (Ann attr (Var "fractionalInt"))
        alg (Ann attr (GetEnv "numDouble" _)) = 
            return $ In (Ann attr (Var "numDouble"))
        alg (Ann attr (GetEnv "fractionalDouble" _)) = 
            return $ In (Ann attr (Var "fractionalDouble"))
        alg (Ann attr (AppClosure e1 e2)) = do
            e1' <- e1
            e2' <- e2
            case (e1', e2') of
                (In (Ann _ (AppClosure l@(In (Ann _ (Var x))) e3)), _) | Set.member x identifierList ->
                    return (In (Ann attr (App (In (Ann attr (App l e3))) e2')))
                (In (Ann _ (Var "==")), In (Ann _ (Var "eqInt"))) ->
                    return $ In (Ann attr (Var "nativeEqInt"))
                (In (Ann _ (Var "+")), In (Ann _ (Var "numInt"))) ->
                    return $ In (Ann attr (Var "nativeAddInt"))
                (In (Ann _ (Var "-")), In (Ann _ (Var "numInt"))) ->
                    return $ In (Ann attr (Var "nativeSubInt"))
                (In (Ann _ (Var "*")), In (Ann _ (Var "numInt"))) ->
                    return $ In (Ann attr (Var "nativeMulInt"))
                (In (Ann _ (Var "/")), In (Ann _ (Var "fractionalInt"))) ->
                    return $ In (Ann attr (Var "nativeDivInt"))
                (In (Ann _ (Var "fromInteger")), In (Ann _ (Var "numInt"))) ->
                    return $ In (Ann attr (Var "nativeInt"))

                (In (Ann _ (Var "==")), In (Ann _ (Var "eqDouble"))) ->
                    return $ In (Ann attr (Var "nativeEqDouble"))
                (In (Ann _ (Var "+")), In (Ann _ (Var "numDouble"))) ->
                    return $ In (Ann attr (Var "nativeAddDouble"))
                (In (Ann _ (Var "-")), In (Ann _ (Var "numDouble"))) ->
                    return $ In (Ann attr (Var "nativeSubDouble"))
                (In (Ann _ (Var "*")), In (Ann _ (Var "numDouble"))) ->
                    return $ In (Ann attr (Var "nativeMulDouble"))
                (In (Ann _ (Var "/")), In (Ann _ (Var "fractionalDouble"))) ->
                    return $ In (Ann attr (Var "nativeDivDouble"))
                (In (Ann _ (Var "fromInteger")), In (Ann _ (Var "numDouble"))) ->
                    return $ In (Ann attr (Var "nativeDouble"))
                (In (Ann _ (Var "fromRational")), In (Ann _ (Var "fractionalDouble"))) ->
                    return $ In (Ann attr (Var "nativeDouble"))
                (In (Ann _ (Var n1)), _) -> do
                    state <- get
                    if member n1 state
                    then do
                        put $ delete n1 state
                        return $ In (Ann attr (AppClosure (state!n1) e2'))
                    else return $ In (Ann attr (AppClosure e1' e2'))
                (_, _) ->
                    return $ In (Ann attr (AppClosure e1' e2'))
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es = evalState (runReaderT ((optimizeImp >=> optimizeImp) es) empty) empty