module DeadCodeElimination where
import Control.Monad.Writer (WriterT (runWriterT), MonadWriter (tell, listen))
import Control.Monad.State (State)
import Data.Set (Set, empty, member, fromList)
import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Control.Monad.State.Lazy (evalState)
import Annotations (Ann(Ann))
import Ast (ExpF(Let, VarPat, Var))

type DeadCodeM = WriterT (Set String) (State (Set String))

optimizeImp :: [TypedExp] -> DeadCodeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (DeadCodeM TypedExp) -> DeadCodeM TypedExp
        alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            (b', vs) <- listen b 
            case n' of 
                In (Ann _ (VarPat k)) -> do
                    if member k vs 
                    then return $ In (Ann attr (Let n' v' b'))
                    else return b'
                _ -> 
                    return $ In (Ann attr (Let n' v' b'))
        alg (Ann attr (Var n)) = do
            tell (fromList [n])
            return $ In (Ann attr (Var n))
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es = fst $ evalState (runWriterT (optimizeImp es)) empty