module DesugarPatternMatching where

import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Annotations (Ann(Ann))
import Ast (ExpF(Var, Match, App, MatchExp, MkTuple, Lam, Lit, ConPat, Let, VarPat))
import Control.Monad.Identity (Identity (runIdentity))
import Location (Loc)
import Types (Qual, Type)
import Primitives (Prim(B))

type PatternM = Identity 

mkList :: (Maybe Loc, Qual Type) -> [TypedExp] -> TypedExp
mkList attr xs = foldr f (In (Ann attr (Var "[]"))) xs
    where f e1 e2 = In (Ann attr (App (In (Ann attr (App (In (Ann attr (Var ":"))) e1))) e2))

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
            case e1' of 
                In (Ann attr (ConPat name [x])) ->
                    return $ In (Ann attr (MkTuple ([In (Ann attr (Var ("is" ++ name))),
                                                     In (Ann attr (Lam (In (Ann attr (VarPat "_v"))) 
                                                       (In (Ann attr (Let x 
                                                       (In (Ann attr (App
                                                       (In (Ann attr (Var ("extract" ++ name)))) (In (Ann attr (Var "_v")))))) e2')))))])))
                _ ->
                    return $ In (Ann attr (MkTuple ([In (Ann attr (Lam e1' (In (Ann attr (Lit (B True)))))),
                                                     In (Ann attr (Lam e1' e2'))])))

        alg x = fmap In (sequenceA x)

desugar :: [TypedExp] -> [TypedExp]
desugar es = runIdentity (desugarImp es)