module ContextReduction where

import Types
import InferMonad
import Unification
import Substitutions
import Monads
import Control.Monad
import Data.Set (toList)

-- Typing Haskell in Haskell Context Reduction
-- https://web.cecs.pdx.edu/~mpj/thih/thih.pdf

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t 
    where hnf (TyVar _ _) = True
          hnf (TyCon _) = False
          hnf (TyLam _ _) = False
          hnf (TyApp t _) = hnf t

tryInst :: Qual Pred -> Pred -> TypeM (Maybe [Pred])
tryInst (ps :=> p') p = catchError 
               (do mguPred p' p 
                   (subs, _) <- get
                   return $ Just (toList (substitutePredicates subs ps))) 
                (const $ return $ Just [])

insts :: [Qual Pred] -> Pred -> [Qual Pred]
insts classEnv p@(IsIn c _) = filter (\(_ :=> (IsIn c2 _)) -> c == c2) classEnv

byInst :: [Qual Pred] -> Pred -> TypeM (Maybe [Pred])
byInst classEnv p = msum [tryInst it p | it <- insts classEnv p]

toHnfs :: [Qual Pred] -> [Pred] -> TypeM [Pred]
toHnfs classEnv ps = do pss <- mapM (toHnf classEnv) ps
                        return (concat pss)

toHnf :: [Qual Pred] -> Pred -> TypeM [Pred]
toHnf classEnv p = do x <- byInst classEnv p 
                      case x of
                       Nothing -> fail "context reduction"
                       Just ps -> toHnfs classEnv ps

                       