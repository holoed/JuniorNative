module ContextReduction where

import Ast ( ExpF(Let, Var) )
import TypedAst ( TypedExp, TypedExpF )
import Fixpoint ( Fix(In) )
import Annotations ( Ann(Ann) )
import Types ( Type(TyApp, TyVar, TyCon), Qual(..), Pred(..), getTVarsOfQPred )
import InferMonad ( TypeM, refreshNames )
import Unification ( mguPred )
import Substitutions ( substitutePredicates, substituteQ, substituteQPred )
import Monads ( get, throwError, catchError )
import Control.Monad.Reader ( msum, runReader, local, ask, Reader )
import Data.Set (Set, toList, fromList, union)
import Data.Maybe ( fromJust, maybeToList )
import RecursionSchemes ( cataRec )
import Location ( Loc, PString(..) )
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
tryInst l (ps :=> p') p =
               do mguPred l p' p
                  (subs, _) <- get
                  return $ Just (toList (substitutePredicates subs ps))

refreshInstance :: Inst -> TypeM Inst
refreshInstance qp = (`substituteQPred` qp) <$> refreshNames tyToRefresh
 where tyToRefresh = getTVarsOfQPred qp

byInst :: Loc -> ClassEnv -> Pred -> TypeM (Maybe [Pred])
byInst l classEnv p@(IsIn i _) = msum [do it' <- refreshInstance it
                                          tryInst l it' p | it <- insts classEnv i]

toHnf :: Loc -> ClassEnv -> Pred -> TypeM [Pred]
toHnf l classEnv p =
    if inHnf p then return [p]
    else do x <- catchError (byInst l classEnv p) (const $ return Nothing)
            case x of
              Nothing -> throwError $ PStr ("Cannot find class instance for " ++ show p, Just l)
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

propagatePreds :: TypedExp -> TypedExp
propagatePreds e = runReader (cataRec alg e) (fromList [])
    where
        alg :: TypedExpF (Reader (Set Pred) TypedExp) -> Reader (Set Pred) TypedExp
        alg (Ann (l, qt@(ps :=> _)) (Let n v b)) = do
            n' <- local (`union` ps) n
            v' <- local (`union` ps) v
            b' <- local (`union` ps) b
            return $ In . Ann (l, qt) $ Let n' v' b'
        alg (Ann (l, ps :=> t) (Var n)) = do
            return $ In (Ann (l, ps :=> t) (Var n))
        alg (Ann (l, ps :=> t) x) = do
            ps' <- ask
            In . Ann (l, (ps `union` ps') :=> t) <$> sequenceA x

resolvePreds :: ClassEnv -> TypedExp -> TypeM TypedExp
resolvePreds classEnv = cataRec alg . propagatePreds
    where alg (Ann (l, qt) x) = do
            (subs, _) <- get
            let (ps :=> t) = substituteQ subs qt
            ps' <- toHnfs (fromJust l) classEnv (toList ps)
            y <- sequenceA (Ann (l, fromList (simplify classEnv ps') :=> t) x)
            return $ In y

