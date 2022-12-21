module Junior.Compiler.DesugarPatternMatching where

import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF)
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Utils.Fixpoint (Fix(In))
import Control.Monad.State (MonadState (get, put))
import Junior.Core.Types (Qual, Type)
import Junior.Utils.Annotations (Ann(Ann))
import Junior.Core.Ast (ExpF(Var, VarPat, Match, MatchExp, ConPat, TuplePat, Lam))
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

merge :: [TypedExp] -> PatternM [TypedExp]
merge xs = do
     var <- getNewVar
     case head xs of  
        (In (Ann attr (MatchExp (In (Ann attr2 (ConPat name args))) target))) -> do
            if length xs == 1
            then do 
                let (ls, _) = partition isAnyConPat args
                exp2 <- if null ls
                then return [mkMatchExp attr (if length ls > 1 then mkTuplePat attr ls else head ls) target]
                else merge [mkMatchExp attr (if length ls > 1 then mkTuplePat attr ls else head ls) target]
                let exp3 = (if not (null ls)
                          then [mkMatch attr (mkVar attr var) exp2]
                          else [target])
                return [In (Ann attr (MatchExp (In (Ann attr2 (ConPat name ((\arg -> if isAnyConPat arg then mkVarPat attr var else arg) <$> args)))) 
                          (head exp3)))]
            else do
                let clauses = (\(In (Ann _ (MatchExp (In (Ann _ (ConPat _ xs))) e2))) -> 
                                In (Ann attr (MatchExp (if length xs == 1 then head xs else mkTuplePat attr xs) e2))) <$> xs
                return [mkMatchExp attr (In (Ann attr2 (ConPat name [mkVarPat attr var]))) (mkMatch attr (mkVar attr var) clauses)]
        _ -> return xs             


desugarLambda :: (Maybe Loc, Qual Type) -> TypedExp -> TypedExp -> PatternM TypedExp
desugarLambda attr e1' e2' = do
            case e1' of
             (In (Ann attr (TuplePat [In (Ann _ (ConPat _ _)), In (Ann _ (ConPat _ _))]))) -> do
              var <- getNewVar
              return $ In (Ann attr (Lam (In (Ann attr (VarPat var))) (In (Ann attr (Match (In (Ann attr (Var var))) [In (Ann attr (MatchExp e1' e2'))])))))  
             (In (Ann _ (ConPat n [In (Ann attr (ConPat n' [In (Ann attr' (VarPat _))]))]))) -> do
              var <- getNewVar
              e2'' <- desugarImp [In (Ann attr (Match (In (Ann attr (Var var))) [In (Ann attr (MatchExp e1' e2'))]))]
              return $ In (Ann attr (Lam (In (Ann attr (VarPat var))) (head e2'')))
             (In (Ann _ (ConPat n [In (Ann attr (VarPat _))]))) -> do
              var <- getNewVar
              return $ In (Ann attr (Lam (In (Ann attr (VarPat var))) (In (Ann attr (Match (In (Ann attr (Var var))) [In (Ann attr (MatchExp e1' e2'))])))))
             _ -> return $ In (Ann attr (Lam e1' e2')) 

desugarImp :: [TypedExp] -> PatternM [TypedExp]
desugarImp = mapM (cataRec alg) 
    where
        alg :: TypedExpF (PatternM TypedExp) -> PatternM TypedExp
        alg (Ann attr (Lam e1 e2)) = do
            e1' <- e1 
            e2' <- e2  
            desugarLambda attr e1' e2'
        alg (Ann attr (Match e1 es1)) = do
            e1' <- e1
            es1' <- sequence es1
            let es2 = collect es1'
            es3 <- mapM merge es2
            return $ In (Ann attr (Match e1' (concat es3)))
        alg (Ann attr (MatchExp e1 e2)) = do
            e1' <- e1
            In . Ann attr . MatchExp e1' <$> e2
        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = fst $ evalRWS (desugarImp es) False 0


