module Junior.Compiler.CompilePatternMatching where

import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF)
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Utils.Fixpoint (Fix(In))
import Junior.Utils.Annotations (Ann(Ann))
import Control.Monad.State (MonadState (get, put), State, evalState)
import Junior.Core.Ast (ExpF(Var, Match, App, MatchExp, MkTuple, Lam, Lit, ConPat, Let, VarPat, TuplePat))
import Junior.Parser.Location (Loc)
import Junior.Core.Types (Qual, Type)
import Junior.Parser.Primitives (Prim(B))

type PatternM = State Int

getNewVar :: PatternM String
getNewVar = do
    i <- get
    put (i + 1)
    return $ "___patV" ++ show i

mkList :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
mkList attr = foldr f (In (Ann attr (Var "[]"))) 
    where f e1 e2 = In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var ":"))) e1))) e2))

mkVarPat :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVarPat attr s = In (Ann attr (VarPat s))

mkVar :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVar attr s = In (Ann attr (Var s))

foldCond :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
foldCond attr = foldl1 (\x y -> In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var "&&"))) x))) y)))

desugarIsPattern :: TypedExp -> PatternM TypedExp
desugarIsPattern (In (Ann attr (ConPat name [In (Ann _ (ConPat name2 []))]))) =
    return $
        In (Ann attr (Lam (In (Ann attr (VarPat "_v")))  
         (foldCond attr [In (Ann attr (App (In (Ann attr (Var ("is" ++ name)))) (mkVar attr "_v"))), 
                         In (Ann attr (App (In (Ann attr (Var ("is" ++ name2)))) (In (Ann attr (App (In (Ann attr (Var ("extract" ++ name)))) (mkVar attr "_v"))))))])))
desugarIsPattern (In (Ann attr (ConPat name _))) =
    return $ In (Ann attr (Var ("is" ++ name)))
desugarIsPattern (In (Ann attr (TuplePat xs))) = do
    xs' <- mapM desugarIsPattern xs
    ps <- mapM (const getNewVar) [1..(length xs)]
    return $ 
     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
     (In (Ann attr (Let (In (Ann attr (TuplePat (mkVarPat attr <$> ps)))) (In (Ann attr (Var "_v")))
             (foldCond attr ((\(s,e) -> In (Ann attr (App e (mkVar attr s)))) <$> zip ps xs')))))))
desugarIsPattern e1'@(In (Ann attr _)) = 
    return $ In (Ann attr (Lam e1' (In (Ann attr (Lit (B True))))))

mergeLets :: TypedExp -> TypedExp -> TypedExp
mergeLets (In (Ann attr1 (Let e1 e2 _))) (In (Ann attr2 (Let e4 e5 e6))) = 
    In (Ann attr1 (Let e1 e2 (In (Ann attr2 (Let e4 e5 e6))))) 
mergeLets e1@(In (Ann _ (Let {}))) _  = e1
mergeLets _  e2@(In (Ann _ (Let {}))) = e2
mergeLets _ _ = error "Unsupported"

extractTupleIndex :: String -> TypedExp -> TypedExp
extractTupleIndex s = cataRec alg
    where alg e@(Ann attr (Var "_v")) = In (Ann attr (App (In (Ann attr (Var s))) (In e)))
          alg e = In e

desugarTarget :: TypedExp -> TypedExp -> PatternM TypedExp
desugarTarget e2 (In (Ann _ (TuplePat [x, y]))) = do
    x' <- desugarTarget e2 x
    y' <- desugarTarget e2 y
    return $ mergeLets (extractTupleIndex "fst" x') (extractTupleIndex "snd" y')
desugarTarget e2 (In (Ann attr (ConPat name [x, y, z, w]))) = do
    return $ In (Ann attr (Let (In (Ann attr (TuplePat [x, y, z, w]))) 
             (In (Ann attr (App
             (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2))
desugarTarget e2 (In (Ann attr (ConPat name [x, y, z]))) = do
    return $ In (Ann attr (Let (In (Ann attr (TuplePat [x, y, z]))) 
             (In (Ann attr (App
             (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2))
desugarTarget e2 (In (Ann attr (ConPat name [x, y]))) = do
    return $ In (Ann attr (Let (In (Ann attr (TuplePat [x, y]))) 
             (In (Ann attr (App
             (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2))
desugarTarget e2 (In (Ann attr (ConPat name [In (Ann attr2 (ConPat _ []))]))) =
    return e2
desugarTarget e2 (In (Ann attr (ConPat name [x]))) =
    return $ In (Ann attr (Let x 
             (In (Ann attr (App
             (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2))
desugarTarget e2 (In (Ann _ (ConPat _ []))) = 
    return e2
desugarTarget e2 e1@(In (Ann attr _)) = 
    return $ In (Ann attr (Let e1 (In (Ann attr (Var "_v"))) e2))

desugarImp :: [TypedExp] -> PatternM [TypedExp]
desugarImp = mapM (cataRec alg) 
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
            return $ In (Ann attr (MkTuple [cond, In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) target))]))
        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = evalState (desugarImp es) 0