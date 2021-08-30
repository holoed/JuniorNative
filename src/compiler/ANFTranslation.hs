module ANFTranslation where

import Location ( Loc )
import TypedAst (TypedExp)
import Ast ( ExpF(Let, Var, App, VarPat, IfThenElse) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Prelude hiding (lookup)
import Annotations (Ann(..))
import Types ( Qual, Type )

type ANormalM = State Int

getNewId :: (Maybe Loc, Qual Type) -> ANormalM (TypedExp, TypedExp)
getNewId attr = do
    i <- get
    put (i + 1)
    let n = "anf_" ++ show i
    return (In (Ann attr (VarPat n)), In (Ann attr (Var n)))

convertProg :: [TypedExp] -> [TypedExp]
convertProg es = evalState (sequence (cataRec alg <$> es)) 0
    where alg (Ann attr (App e1 e2)) = do
                e1' <- e1
                e2' <- e2
                case (e1', e2') of
                 (In (Ann _ (Var _)), In (Ann _ (Var _))) ->  
                     return (In (Ann attr (App e1' e2')))
                 (In (Ann _ (Var _)), _) -> do
                     (n2p, n2v) <- getNewId attr
                     return 
                        (In (Ann attr (Let n2p e2'
                        (In (Ann attr (App e1' n2v))))))
                 (_, In (Ann _ (Var _))) -> do
                     (n1p, n1v) <- getNewId attr
                     return
                        (In (Ann attr (Let n1p e1'
                        (In (Ann attr (App n1v e2'))))))
                 (_, _) -> do 
                     (n1p, n1v) <- getNewId attr
                     (n2p, n2v) <- getNewId attr
                     return
                        (In (Ann attr (Let n1p e1'
                        (In (Ann attr (Let n2p e2'
                        (In (Ann attr (App n1v n2v)))))))))
          alg (Ann attr (IfThenElse e1 e2 e3)) = do
                     e1' <- e1
                     e2' <- e2
                     e3' <- e3
                     case e1' of 
                        (In (Ann _ (Var _))) -> 
                          return $ In (Ann attr (IfThenElse e1' e2' e3'))
                        (In (Ann attr' (Let n v b))) -> do
                          (n1p, n1v) <- getNewId attr'
                          return 
                            (In (Ann attr' (Let n v
                            (In (Ann attr' (Let n1p b
                            (In (Ann attr (IfThenElse n1v e2' e3')))))))))
                        _ -> do 
                            (n1p, n1v) <- getNewId attr
                            return
                             (In (Ann attr (Let n1p e1'
                             (In (Ann attr (IfThenElse n1v e2' e3'))))))
          alg x = fmap In (sequenceA x)
