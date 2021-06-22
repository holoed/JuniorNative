module ContextReduction where

import TypedAst ( TypedExp )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Types ( Type(TyApp, TyVar, TyCon), Qual(..), Pred(..) )
import InferMonad ( TypeM )
import Unification ( mguPred )
import Substitutions ( substitutePredicates, substituteQ )
import Monads ( get, throwError, catchError )
import Control.Monad ( msum )
import Data.Set (toList, fromList)
import RecursionSchemes ( cataRec )
import Ast (ExpLoc, extractLoc)

-- Typing Haskell in Haskell Context Reduction
-- https://web.cecs.pdx.edu/~mpj/thih/thih.pdf

inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
    where hnf (TyVar _ _) = True
          hnf (TyCon _) = False
          hnf (TyApp t' _) = hnf t'

tryInst :: ExpLoc -> Qual Pred -> Pred -> TypeM (Maybe [Pred])
tryInst l (ps :=> p') p = catchError
               (do mguPred (extractLoc l) p' p
                   (subs, _) <- get
                   return $ Just (toList (substitutePredicates subs ps)))
                (const $ return $ Just [])

insts :: [Qual Pred] -> Pred -> [Qual Pred]
insts classEnv (IsIn c _) = filter (\(_ :=> (IsIn c2 _)) -> c == c2) classEnv

byInst :: ExpLoc -> [Qual Pred] -> Pred -> TypeM (Maybe [Pred])
byInst l classEnv p = msum [tryInst l it p | it <- insts classEnv p]

toHnf :: ExpLoc -> [Qual Pred] -> Pred -> TypeM [Pred]
toHnf l classEnv p = if inHnf p then return [p]
                     else do x <- catchError (byInst l classEnv p) (const $ return $ Nothing)
                             case x of
                               Nothing -> throwError $ "Cannot find class instance for " ++ show p
                               Just ps -> toHnfs l classEnv ps

toHnfs :: ExpLoc -> [Qual Pred] -> [Pred] -> TypeM [Pred]
toHnfs l classEnv ps = do pss <- mapM (toHnf l classEnv) ps
                          return (concat pss)

resolvePreds :: [Qual Pred] -> TypedExp -> TypeM TypedExp
resolvePreds classEnv =  cataRec alg
    where alg (Ann (l, qt) x) = do 
            (subs, _) <- get
            let (ps :=> t) = substituteQ subs qt
            ps' <- toHnfs l classEnv (toList ps)
            y <- sequenceA (Ann (l, fromList ps' :=> t) x)
            return $ In y