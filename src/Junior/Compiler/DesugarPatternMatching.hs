module Junior.Compiler.DesugarPatternMatching where

import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF)
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Utils.Fixpoint (Fix(In))
import Control.Monad.State (MonadState (get, put))
import Junior.Core.Types (Qual, Type)
import Junior.Utils.Annotations (Ann(Ann))
import Junior.Core.Ast (ExpF(Var, VarPat, Match, MatchExp, ConPat, TuplePat))
import Junior.Parser.Location (Loc(..))
import Control.Monad.RWS (RWS, evalRWS)
import Data.List (partition)

type PatternM = RWS Bool [(TypedExp, TypedExp)] Int

getNewVar :: PatternM String
getNewVar = do
    i <- get
    put (i + 1)
    return $ "___patV" ++ show i

mkVar :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVar attr s = In (Ann attr (Var s))

mkVarPat :: (Maybe Loc, Qual Type) -> String -> TypedExp
mkVarPat attr s = In (Ann attr (VarPat s))

mkTuplePat :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
mkTuplePat attr ps = In (Ann attr (TuplePat ps))

mkMatch :: (Maybe Loc, Qual Type) -> TypedExp -> [TypedExp] -> TypedExp
mkMatch attr e es = In (Ann attr (Match e es))

mkMatchExp :: (Maybe Loc, Qual Type) -> TypedExp -> TypedExp -> TypedExp
mkMatchExp attr e1 e2 = In (Ann attr (MatchExp e1 e2))

isConPat :: String -> TypedExp -> Bool
isConPat name1 (In (Ann _ (MatchExp (In (Ann _ (ConPat name2 _))) _))) = name1 == name2 
isConPat _ _ = False

isAnyConPat :: TypedExp -> Bool
isAnyConPat (In (Ann _ (ConPat _ _))) = True
isAnyConPat _ = False

collect :: [TypedExp] -> [[TypedExp]]
collect vs@(x:xs) = 
    case x of 
      In (Ann _ (MatchExp (In (Ann _ (ConPat name _))) _)) -> 
        let (ls, rs) = partition (isConPat name) xs in
        (x : ls) : collect rs
      _ -> [vs]
collect [] = []

merge :: [TypedExp] -> PatternM TypedExp
merge xs = do
     var <- getNewVar
     case head xs of  
        (In (Ann attr (MatchExp (In (Ann attr2 (ConPat name args))) target))) -> do
            if (length xs == 1) 
            then do 
                let (ls, rs) = partition isAnyConPat args
                return $ (In (Ann attr (MatchExp (In (Ann attr2 (ConPat name (((\arg -> if (isAnyConPat arg) then (mkVarPat attr var) else arg) <$> args))))) 
                          (if (length ls > 0) 
                          then (mkMatch attr (mkVar attr var) 
                               [mkMatchExp attr (if (length ls > 1) then mkTuplePat attr ls else (head ls)) target])
                          else target))))
            else do
                let clauses = (\(In (Ann _ (MatchExp (In (Ann _ (ConPat _ xs))) e2))) -> 
                                (In (Ann attr (MatchExp (if (length xs == 1) then (head xs) else mkTuplePat attr xs) e2)))) <$> xs
                return $ mkMatchExp attr (In (Ann attr2 (ConPat name [mkVarPat attr var]))) (mkMatch attr (mkVar attr var) clauses)
        _ -> return $ head xs             

desugarImp :: [TypedExp] -> PatternM [TypedExp]
desugarImp es = sequence (cataRec alg <$> es)
    where
        alg :: TypedExpF (PatternM TypedExp) -> PatternM TypedExp
        alg (Ann attr (Match e1 es1)) = do
            e1' <- e1
            es1' <- sequence es1
            let es2 = collect es1'
            es3 <- sequence (merge <$> es2)
            return $ In (Ann attr (Match e1' es3))
        alg (Ann attr (MatchExp e1 e2)) = do
            e1' <- e1 
            e2' <- e2
            return $ In (Ann attr (MatchExp e1' e2'))
        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = fst $ evalRWS (desugarImp es) False 0

