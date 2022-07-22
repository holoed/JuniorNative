module Junior.Compiler.OptimizeClosureEnvs where

import Data.Set ( Set, fromList, member )
import Data.Map ( Map, empty, insert, (!) )
import Control.Monad.Trans.Writer ( tell, listen, WriterT(runWriterT) )
import Control.Monad.State ( runState, State, MonadState(put, get) )
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF)
import Junior.Utils.Annotations (Ann(Ann))
import Junior.Core.Ast (ExpF(Let, VarPat, GetEnv, Defn, SetEnv, MkClosure, Var, TuplePat))
import Junior.Utils.Fixpoint (Fix(In))
import Control.Monad ((>=>))

type OptimizeM = WriterT (Set String) (State (Map String (Set String)))

extractNames :: TypedExp -> [String]
extractNames (In (Ann _ (VarPat s))) = [s]
extractNames (In (Ann _ (TuplePat es))) = es >>= extractNames
extractNames _ = error "Unsupported"

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
                (In (Ann _ (Var n))) | not (member k (state!n)) -> return e2'
                (In (Ann _ (SetEnv _ _ (In (Ann _ (Var n)))))) | not (member k (state!n)) -> return e2'
                (In (Ann _ (SetEnv _ _ (In (Ann _ (SetEnv _ _ (In (Ann _ (Var n))))))))) | not (member k (state!n)) -> return e2'
                _ -> return $ In (Ann attr (SetEnv k e1' e2'))
        alg (Ann attr (MkClosure n)) = do
            state <- get
            tell (state!n)
            return $ In (Ann attr (MkClosure n))
        alg (Ann attr (Let n v b)) = do
            n' <- n
            (v', ns) <- listen v
            state <- get
            let ks = extractNames n'
            _ <- put (foldr (`insert` ns) state ks)
            In . Ann attr . Let n' v' <$> b
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es =
    let ((es', _), _) = runState (runWriterT ((optimizeImp >=> optimizeImp) es)) empty in es'