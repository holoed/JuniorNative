module DeadCodeElimination where
import Control.Monad.Writer (WriterT (runWriterT))
import Control.Monad.State (State)
import Data.Set (Set, empty)
import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Control.Monad.State.Lazy (evalState)
import Annotations (Ann(Ann))
import Ast (ExpF(Let))
import Control.Monad.Writer.Class (MonadWriter(listen))

type DeadCodeM = WriterT (Set String) (State (Set String))

optimizeImp :: [TypedExp] -> DeadCodeM [TypedExp]
optimizeImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (DeadCodeM TypedExp) -> DeadCodeM TypedExp
        alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            (b', vs) <- listen b 
            return $ In (Ann attr (Let n' v' b'))
        alg x = fmap In (sequenceA x)

optimize :: [TypedExp] -> [TypedExp]
optimize es = fst $ evalState (runWriterT (optimizeImp es)) empty