module Junior.Compiler.ANFTranslation where

import Junior.Parser.Location ( Loc )
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Core.Ast ( ExpF(Let, Var, App, VarPat, IfThenElse, MkTuple) )
import Control.Monad.State ( evalState, State, MonadState(put, get) )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Utils.RecursionSchemes ( cataRec )
import Prelude hiding (lookup)
import Junior.Utils.Annotations (Ann(..))
import Junior.Core.Types ( Qual, Type )
import Control.Monad (foldM)

type ANormalM = State Int

getNewId :: (Maybe Loc, Qual Type) -> ANormalM (TypedExp, TypedExp)
getNewId attr = do
    i <- get
    put (i + 1)
    let n = "anf_" ++ show i
    return (In (Ann attr (VarPat n)), In (Ann attr (Var n)))

convertProg :: [TypedExp] -> [TypedExp]
convertProg es = linearizeLets $ evalState (sequence (cataRec alg <$> es)) 0
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
          alg (Ann attr (MkTuple xs)) = do
                    xs' <- sequence xs
                    ids <- sequence $ getNewId attr <$ [1..length xs']
                    let b' = In (Ann attr (MkTuple (snd <$> ids)))
                    let pairs = zip (fst <$> ids) xs'
                    foldM (\b (n, v) -> return $ In (Ann attr (Let n v b))) b' pairs
          alg x = fmap In (sequenceA x)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = undefined

linearizeLets :: [TypedExp] -> [TypedExp]
linearizeLets = converge (==) . iterate (reduce <$>)
    where reduce = cataRec alg
          alg (Ann attr (Let n1 (In (Ann _ (Let n2 v b1))) b2)) =
               In (Ann attr (Let n2 v
               (In (Ann attr (Let n1 b1 b2)))))
          alg x = In x

