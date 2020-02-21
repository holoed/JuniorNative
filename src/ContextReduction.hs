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
tryInst (ps :=> p') p = catchError 
               (do mguPred p' p 
                   (subs, _) <- get
                   return $ Just (substitutePredicates subs ps)) 
                (const $ return $ Just empty)

insts :: Set (Qual Pred) -> Pred -> Set (Qual Pred)
insts classEnv p@(IsIn c _) = 
    Data.Set.filter (\(_ :=> (IsIn c2 _)) -> c == c2) classEnv

-- byInst :: Set (Qual Pred) -> Pred -> Maybe (Set Pred)
-- byInst classEnv p@(IsIn c _) =
--     let ps = Data.Set.filter (\Qual _ (IsIn c2 _) -> c == c2) classEnv

