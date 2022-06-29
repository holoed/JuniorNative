module DesugarPatternMatching where

import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Annotations (Ann(Ann))
import Ast (ExpF(Var, Match, App, MatchExp, MkTuple, Lam, Lit, ConPat, Let, VarPat, TuplePat))
import Control.Monad.State (State, evalState, MonadState (get, put))
import Location (Loc)
import Types (Qual, Type)
import Primitives (Prim(B))

type PatternM = State Int

getNewVar :: PatternM String
getNewVar = do
    x <- get
    put (x + 1)
    return $ "___w" ++ show x

replaceConstWithWildCard :: TypedExp -> PatternM TypedExp
replaceConstWithWildCard (In (Ann attr (ConPat _ []))) = do
    x <- getNewVar
    return (In (Ann attr (VarPat x))) 
replaceConstWithWildCard (In (Ann attr (ConPat name xs))) = do
    xs' <- sequence $ replaceConstWithWildCard <$> xs
    return (In (Ann attr (ConPat name xs'))) 
replaceConstWithWildCard x = return x

mkList :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
mkList attr xs = foldr f (In (Ann attr (Var "[]"))) xs
    where f e1 e2 = In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var ":"))) e1))) e2))

desugarIsPattern :: TypedExp -> PatternM TypedExp
desugarIsPattern (In (Ann attr (ConPat name [In (Ann _ (ConPat name2 _))]))) =  
    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
            (In (Ann attr (App 
            (In (Ann attr (App (In (Ann attr (Var "&&")))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name)))) (In (Ann attr (Var "_v")))))))))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name2))))
            (In (Ann attr (App
            (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v"))))))))))))))
desugarIsPattern (In (Ann attr (ConPat name [_, In (Ann _ (ConPat name2 [_, _]))]))) =
    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
            (In (Ann attr (App 
            (In (Ann attr (App (In (Ann attr (Var "&&")))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name)))) (In (Ann attr (Var "_v")))))))))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name2))))
            (In (Ann attr (App
            (In (Ann attr (Var ("snd"))))
            (In (Ann attr (App
            (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))))))))))))))
desugarIsPattern (In (Ann attr (ConPat name [_, In (Ann _ (ConPat name2 []))]))) =
    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
            (In (Ann attr (App 
            (In (Ann attr (App (In (Ann attr (Var "&&")))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name)))) (In (Ann attr (Var "_v")))))))))
            (In (Ann attr (App (In (Ann attr (Var ("is" ++ name2))))
            (In (Ann attr (App
            (In (Ann attr (Var ("snd"))))
            (In (Ann attr (App
            (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))))))))))))))
desugarIsPattern (In (Ann attr (ConPat name [_, _]))) = 
    return $ In (Ann attr (Var ("is" ++ name)))
desugarIsPattern (In (Ann attr (ConPat name [_]))) =
    return $ In (Ann attr (Var ("is" ++ name)))
desugarIsPattern (In (Ann attr (ConPat name []))) =
    return $ In (Ann attr (Var ("is" ++ name)))
desugarIsPattern e1'@(In (Ann attr _)) = 
    return $ In (Ann attr (Lam e1' (In (Ann attr (Lit (B True))))))


desugarImp :: [TypedExp] -> PatternM [TypedExp]
desugarImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (PatternM TypedExp) -> PatternM TypedExp
        alg (Ann attr (Match e' es')) = do 
            e'' <- e'
            es'' <- sequence es'
            return $ In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var "matchFn"))) (mkList attr es'')))) e''))
        alg (Ann attr (MatchExp e1 e2)) = do
            e1' <- e1 
            e2' <- e2
            cond <- desugarIsPattern e1'
            case e1' of 
                In (Ann _ (ConPat _ [In (Ann _ (ConPat _ []))])) -> 
                    return $ In (Ann attr (MkTuple ([cond,
                                                    In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) e2'))])))
                In (Ann _ (ConPat name [In (Ann _ (ConPat name2 [x]))])) -> do
                    x' <- replaceConstWithWildCard x
                    return $ In (Ann attr (MkTuple ([cond,
                                                    In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let (In (Ann attr (VarPat ("____x")))) 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var ("_v")))))))
                                                       (In (Ann attr (Let x' 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name2)))) (In (Ann attr (Var "____x"))))))
                                                        e2'))))))))])))
                In (Ann _ (ConPat name [x, In (Ann _ (ConPat name2 [y, z]))])) -> do
                    x' <- replaceConstWithWildCard x
                    y' <- replaceConstWithWildCard y
                    z' <- replaceConstWithWildCard z
                    return $ In (Ann attr (MkTuple ([cond,
                                                    In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let (In (Ann attr (TuplePat [x', (In (Ann attr (VarPat "____yz")))]))) 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v"))))))
                                                       (In (Ann attr (Let (In (Ann attr (TuplePat [y', z']))) 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name2)))) (In (Ann attr (Var "____yz"))))))
                                                        e2'))))))))])))
                In (Ann _ (ConPat name [x, y])) -> do
                    x' <- replaceConstWithWildCard x
                    y' <- replaceConstWithWildCard y
                    return $ In (Ann attr (MkTuple ([cond,
                                                     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let (In (Ann attr (TuplePat [x', y']))) 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2')))))])))
                In (Ann _ (ConPat name [x])) -> 
                    return $ In (Ann attr (MkTuple ([cond,
                                                     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let x 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2')))))])))
                In (Ann _ (ConPat _ [])) -> 
                    return $ In (Ann attr (MkTuple ([cond,
                                                     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) e2'))])))
                _ -> return $ In (Ann attr (MkTuple ([cond,
                                                     In (Ann attr (Lam e1' e2'))])))

        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = evalState (desugarImp es) 0