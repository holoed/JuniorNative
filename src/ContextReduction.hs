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
import Data.Maybe ( fromJust, maybeToList )
import RecursionSchemes ( cataRec )
import Ast (Loc)
import Data.Map (Map, (!?))

-- Typing Haskell in Haskell Context Reduction
-- https://web.cecs.pdx.edu/~mpj/thih/thih.pdf

type Inst = Qual Pred
type Class = ([String], [Inst])

data ClassEnv = ClassEnv {
    classes :: Map String Class,
    defaults :: [Type]
    }

super :: ClassEnv -> String -> [String]
super ce i = maybeToList (classes ce!?i) >>= fst

insts :: ClassEnv -> String -> [Inst]
insts ce i = maybeToList (classes ce!?i) >>= snd

inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
    where hnf (TyVar _ _) = True
          hnf (TyCon _) = False
          hnf (TyApp t' _) = hnf t'

tryInst :: Loc -> Qual Pred -> Pred -> TypeM (Maybe [Pred])
tryInst l (ps :=> p') p = catchError
               (do mguPred l p' p
                   (subs, _) <- get
                   return $ Just (toList (substitutePredicates subs ps)))
                (const $ return $ Just [])

byInst :: Loc -> ClassEnv -> Pred -> TypeM (Maybe [Pred])
byInst l classEnv p@(IsIn i _) = msum [tryInst l it p | it <- insts classEnv i]

toHnf :: Loc -> ClassEnv -> Pred -> TypeM [Pred]
toHnf l classEnv p = if inHnf p then return [p]
                     else do x <- catchError (byInst l classEnv p) (const $ return $ Nothing)
                             case x of
                               Nothing -> throwError $ "Cannot find class instance for " ++ show p
                               Just ps -> toHnfs l classEnv ps

toHnfs :: Loc -> ClassEnv -> [Pred] -> TypeM [Pred]
toHnfs l classEnv ps = do pss <- mapM (toHnf l classEnv) ps
                          return (concat pss)

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
    p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any ((p `elem`) . bySuper ce) ps

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop [ ]
    where loop rs [ ] = rs
          loop rs (p : ps) | entail ce (rs ++ ps) p = loop rs ps
            | otherwise = loop (p : rs) ps

resolvePreds :: ClassEnv -> TypedExp -> TypeM TypedExp
resolvePreds classEnv = cataRec alg
    where alg (Ann (l, qt) x) = do
            (subs, _) <- get
            let (ps :=> t) = substituteQ subs qt
            ps' <- toHnfs (fromJust l) classEnv (toList ps)
            y <- sequenceA (Ann (l, fromList (simplify classEnv ps') :=> t) x)
            return $ In y