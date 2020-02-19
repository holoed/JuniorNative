module ContextReduction where

import Types
import InferMonad
import Unification
import Substitutions
import Monads
import Data.Set

-- Typing Haskell in Haskell Context Reduction
-- https://web.cecs.pdx.edu/~mpj/thih/thih.pdf

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t 
    where hnf (TyVar _ _) = True
          hnf (TyCon _) = False
          hnf (TyLam _ _) = False
          hnf (TyApp t _) = hnf t

tryInst :: Qual Pred -> Pred -> TypeM (Maybe (Set Pred))
tryInst qp p = catchError 
               (do let (ps :=> p') = qp
                   mguPred p' p 
                   (subs, _) <- get
                   return $ Just (substitutePredicates subs ps)) (const $ return $ Just empty)

