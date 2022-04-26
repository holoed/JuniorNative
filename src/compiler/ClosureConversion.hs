{-# LANGUAGE FlexibleContexts #-}
module ClosureConversion where

import Location (zeroLoc)
import Annotations ( Ann(Ann), mapAnn )
import Fixpoint ( Fix(In) )
import Ast ( ExpF(Var, VarPat, TuplePat, Lam, App, SetEnv, GetEnv, Var, Let, MkClosure, Defn, AppClosure) )
import Data.Set ( Set, toList, member, fromList, delete, union )
import Control.Monad.RWS.Lazy ( RWS, runRWS )
import Control.Monad.Writer ( MonadWriter(tell) )
import Control.Monad.State ( MonadState(put, get) )
import Control.Monad.Reader ( MonadReader(local, ask), runReader )
import RecursionSchemes (cataRec)
import FreeVariables (FreeVarsExp, freeVars)
import Types (Qual, Type)
import TypedAst (TypedExp)
import Data.Bifunctor (second)

type TypedFExp = FreeVarsExp (Qual Type)
type ClosureM = RWS (Set String) [TypedFExp] (Int, Int)

convertProg :: Set String -> [TypedExp] -> [TypedExp]
convertProg existing defs = mapAnn (\(qt, _) -> (Just zeroLoc, qt)) <$> newDefs ++ origDefs
  where
    topLevelNames = fromList . map (\(In (Ann _ (Defn _ (In (Ann _ (VarPat name))) _))) -> name) $ defs
    globals = topLevelNames `union` existing
    newDefsM = sequence (
       (\(In (Ann (_, qt) (Defn _ (In (Ann (_, qt2) (VarPat n))) b))) ->
          do b' <- convertBody globals b
             return $ In (Ann (qt, fromList []) (Defn Nothing (In (Ann (qt2, fromList []) (VarPat n))) b'))) <$> defs)
    (origDefs, _, newDefs) = runRWS newDefsM globals (0, 0)

convertBody :: Set String -> Fix (Ann (a, Qual Type) ExpF) -> ClosureM TypedFExp
convertBody globals = convert . freeVars globals . mapAnn snd

freshFunc :: ClosureM String
freshFunc = do
  (f, c) <- get
  put (f+1, c)
  return $ "_f" ++ show f

freshClos :: ClosureM String
freshClos = do
  (f, c) <- get
  put (f, c+1)
  return $ "_c" ++ show c

setEnv :: (Qual Type, Set String) -> String -> TypedFExp -> TypedFExp
setEnv attr name x = In (Ann attr (SetEnv name (In (Ann attr (Var name))) x))

convert :: TypedFExp -> ClosureM TypedFExp
convert = cataRec alg
    where
     alg (Ann attr@(_,freeVars') (Lam e1 e2)) = do
            arg@(In (Ann varAttr _)) <- e1
            name <- freshFunc
            e2' <- e2
            let newArg = In (Ann varAttr (VarPat "_env"))
            let newArgRef = In (Ann varAttr (Var "_env"))
            tell [In (Ann attr (Defn Nothing
                                     (In (Ann attr (VarPat name)))
                                     (In (Ann attr (Lam (In (Ann varAttr (TuplePat [newArg, arg])))
                                                        (subst freeVars' newArgRef e2'))))))]
            closName <- freshClos
            let envBindings = foldr (setEnv attr) (In (Ann attr (Var closName))) (toList freeVars')
            return $ In (Ann attr (Let (In (Ann attr (VarPat closName)))
                                       (In (Ann attr (MkClosure name)))
                                       envBindings))

     alg (Ann attr (App e1 e2)) = do
           e1' <- e1
           callClosure attr e1' <$> e2
     alg (Ann _ Defn {}) = undefined
     alg x = fmap In (sequenceA x)

extractNames :: TypedFExp -> [String]
extractNames (In (Ann _ (VarPat s))) = [s]
extractNames (In (Ann _ (TuplePat es))) = es >>= extractNames 
extractNames _ = error "Unsupported"

subst :: Set String -> TypedFExp -> TypedFExp -> TypedFExp
subst vars env expr = runReader (cataRec alg expr) (env, vars)
   where alg (Ann attr (Let n v b)) = do
            n' <- n
            let names = extractNames n'
            v' <- v
            b' <- local (second (\ctx -> foldr delete ctx names)) b
            return $ In (Ann attr (Let n' v' b'))
         alg (Ann attr (Var name)) = do
            (env', vars') <- ask
            if member name vars'
            then return $ In (Ann attr (GetEnv name env'))
            else return $ In (Ann attr (Var name))
         alg (Ann attr (Lam e1 e2)) = do
            e1' <- e1
            let In (Ann _ (VarPat name)) = e1'
            e2' <- local (second (delete name)) e2
            return $ In (Ann attr (Lam e1' e2'))
         alg (Ann _ Defn {}) = undefined
         alg x = fmap In (sequenceA x)

callClosure ::  (Qual Type, Set String) -> TypedFExp -> TypedFExp -> TypedFExp
callClosure attr closure arg =
 In (Ann attr (AppClosure closure arg))

