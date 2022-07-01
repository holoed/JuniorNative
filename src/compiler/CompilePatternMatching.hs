module CompilePatternMatching where

import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Annotations (Ann(Ann))
import Control.Monad.State (MonadState (get, put))
import Ast (ExpF(Var, Match, App, MatchExp, MkTuple, Lam, Lit, ConPat, Let, VarPat, TuplePat))
import Control.Monad.State (State, evalState)
import Location (Loc)
import Types (Qual, Type)
import Primitives (Prim(B))

type PatternM = State Int

getNewVar :: PatternM String
getNewVar = do
    i <- get
    put (i + 1)
    return $ "___patV" ++ show i

mkList :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
mkList attr xs = foldr f (In (Ann attr (Var "[]"))) xs
    where f e1 e2 = In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var ":"))) e1))) e2))

mkVarPat :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVarPat attr s = In (Ann attr (VarPat s))

mkVar :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVar attr s = In (Ann attr (Var s))

foldCond :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
foldCond attr = foldl1 (\x y -> (In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var "&&"))) x))) y))))

desugarIsPattern :: TypedExp -> PatternM TypedExp
desugarIsPattern (In (Ann attr (ConPat name _))) =
    return $ In (Ann attr (Var ("is" ++ name)))
desugarIsPattern (In (Ann attr (TuplePat xs))) = do
    xs' <- sequence (desugarIsPattern <$> xs)
    ps <- sequence ((\_ -> getNewVar) <$> [1..(length xs)]) 
    return $ 
     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
     (In (Ann attr (Let (In (Ann attr (TuplePat ((mkVarPat attr) <$> ps)))) (In (Ann attr (Var "_v")))
             (foldCond attr ((\(s,e) -> (In (Ann attr (App e (mkVar attr s))))) <$> zip ps xs')))))))
desugarIsPattern e1'@(In (Ann attr _)) = 
    return $ In (Ann attr (Lam e1' (In (Ann attr (Lit (B True))))))

desugarTarget :: TypedExp -> TypedExp -> PatternM TypedExp
desugarTarget e2 (In (Ann attr (TuplePat [x, y]))) = do
                    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let (In (Ann attr (TuplePat [x, y]))) 
                                                       (In (Ann attr (Var "_v"))) e2)))))
desugarTarget e2 (In (Ann attr (ConPat name [x, y]))) = do
                    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let (In (Ann attr (TuplePat [x, y]))) 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2)))))
desugarTarget e2 (In (Ann attr (ConPat name [x]))) =
                    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let x 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2)))))
desugarTarget e2 (In (Ann attr (ConPat _ []))) =
                    return $ In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) e2))
desugarTarget e2 e1@(In (Ann attr _)) = return $ In (Ann attr (Lam e1 e2))


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
            target <- desugarTarget e2' e1'
            return $ In (Ann attr (MkTuple ([cond, target])))
        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = evalState (desugarImp es) 0