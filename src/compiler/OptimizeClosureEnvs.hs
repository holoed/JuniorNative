module OptimizeClosureEnvs where

import Data.Set ( Set, size, fromList, member )
import Data.Map ( Map, empty, insert, (!) )
import Control.Monad.Trans.Writer ( tell, listen, WriterT(runWriterT) )
import Control.Monad.State ( runState, State, MonadState(put, get) )
import RecursionSchemes (cataRec)
import TypedAst (TypedExp, TypedExpF)
import Annotations (Ann(Ann))
import Ast (ExpF(Let, VarPat, GetEnv, Defn, SetEnv, MkClosure, Var))
import Fixpoint (Fix(In))
import Control.Monad ((>=>))
import Debug.Trace (trace)

type OptimizeM = WriterT (Set String) (State (Map String (Set String)))

optimizeImp :: [TypedExp] -> OptimizeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (OptimizeM TypedExp) -> OptimizeM TypedExp
        alg (Ann attr (Defn qt e1 e2)) = do
            e1' <- e1
            let (In (Ann _ (VarPat k))) = e1'
            (e2', ns) <- listen e2
            state <- get
            _ <- put (insert k ns state)
            return $ In (Ann attr (Defn qt e1' e2'))
        alg (Ann attr (GetEnv k e)) =  do
            e' <- e
            _ <- tell (fromList [k])
            return $ In (Ann attr (GetEnv k e'))
        alg (Ann attr (SetEnv k e1 e2)) =  do
            e1' <- e1
            e2' <- e2
            state <- get
            case e2' of
                (In (Ann _ (Var n))) | not (member k (state!n)) -> return (trace ("Removed SetEnv " ++ k ++ " " ++ n) e2')
                (In (Ann _ (SetEnv _ _ (In (Ann _ (Var n)))))) | not (member k (state!n)) -> return (trace ("Removed SetEnv " ++ k ++ " " ++ n) e2')
                _ -> return $ In (Ann attr (SetEnv k e1' e2'))
        alg (Ann attr (MkClosure n)) = do
            state <- get
            tell (state!n)
            return $ In (Ann attr (MkClosure n))
        alg (Ann attr (Let n v b)) = do
            n' <- n
            (v', ns) <- listen v
            state <- get
            let (In (Ann attr (VarPat k))) = n'
            _ <- put (insert k ns state)
            In . Ann attr . Let n' v' <$> b
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es =
    let ((es', _), ns) = runState (runWriterT ((optimizeImp >=> optimizeImp) es)) empty in es'