module ContextReduction where

import TypedAst
import Fixpoint
import Annotations
import Types
import InferMonad
import Unification
import Substitutions
import Monads
import Control.Monad
import Data.Set (toList, fromList)
import RecursionSchemes

-- Typing Haskell in Haskell Context Reduction
-- https://web.cecs.pdx.edu/~mpj/thih/thih.pdf

inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t 
    where hnf (TyVar _ _) = True
          hnf (TyCon _) = False
          hnf (TyApp t' _) = hnf t'

tryInst :: Qual Pred -> Pred -> TypeM (Maybe [Pred])
tryInst (ps :=> p') p = catchError 
               (do mguPred p' p 
                   (subs, _) <- get
                   return $ Just (toList (substitutePredicates subs ps))) 
                (const $ return $ Just [])

insts :: [Qual Pred] -> Pred -> [Qual Pred]
insts classEnv (IsIn c _) = filter (\(_ :=> (IsIn c2 _)) -> c == c2) classEnv

byInst :: [Qual Pred] -> Pred -> TypeM (Maybe [Pred])
byInst classEnv p = msum [tryInst it p | it <- insts classEnv p]

toHnf :: [Qual Pred] -> Pred -> TypeM [Pred]
toHnf classEnv p = if (inHnf p) then return [p]
                   else do x <- catchError (byInst classEnv p) (const $ return $ Nothing)
                           case x of
                             Nothing -> throwError $ "Cannot find class instance for " ++ (show p)
                             Just ps -> toHnfs classEnv ps

toHnfs :: [Qual Pred] -> [Pred] -> TypeM [Pred]
toHnfs classEnv ps = do pss <- mapM (toHnf classEnv) ps
                        return (concat pss)                      

resolvePreds :: [Qual Pred] -> TypedExp -> TypeM TypedExp
resolvePreds classEnv =  cataRec alg
    where alg (Ann qt x) = do (subs, _) <- get
                              let (ps :=> t) = substituteQ subs qt
                              ps' <- toHnfs classEnv (toList ps)
                              y <- traverse id (Ann (fromList ps' :=> t) x)
                              return $ In y