{-# LANGUAGE FlexibleContexts #-}
module Junior.Compiler.ClosureConversion where

import Junior.Parser.Location (zeroLoc)
import Junior.Utils.Annotations ( Ann(Ann), mapAnn )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Core.Ast ( ExpF(Var, VarPat, TuplePat, Lam, App, SetEnv, GetEnv, Var, Let, MkClosure, Defn, AppClosure) )
import Data.Set ( Set, toList, member, fromList, delete, union )
import Control.Monad.RWS.Lazy ( RWS, runRWS )
import Control.Monad.Writer ( MonadWriter(tell) )
import Control.Monad.State ( MonadState(put, get) )
import Control.Monad.Reader ( MonadReader(local, ask), runReader )
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Compiler.FreeVariables (FreeVarsExp, freeVars)
import Junior.Core.Types (Qual, Type)
import Junior.TypeChecker.TypedAst (TypedExp)
import Data.Bifunctor (second)
import Debug.Trace (trace)

type TypedFExp = FreeVarsExp (Qual Type)
type ClosureM = RWS (Set String) [TypedFExp] (String, Int, Int)

convertProg :: String -> Set String -> [TypedExp] -> [TypedExp]
convertProg ns existing defs = removeRedundantLet $ mapAnn (\(qt, _) -> (Just zeroLoc, qt)) <$> newDefs ++ origDefs
  where
    topLevelNames = fromList . map (\(In (Ann _ (Defn _ (In (Ann _ (VarPat name))) _))) -> name) $ defs
    globals = topLevelNames `union` existing
    newDefsM = sequence (
       (\(In (Ann (_, qt) (Defn _ (In (Ann (_, qt2) (VarPat n))) b))) ->
          do b' <- convertBody globals b
             return $ In (Ann (qt, fromList []) (Defn Nothing (In (Ann (qt2, fromList []) (VarPat n))) b'))) <$> defs)
    (origDefs, _, newDefs) = runRWS newDefsM globals (ns, 0, 0)

convertBody :: Set String -> Fix (Ann (a, Qual Type) ExpF) -> ClosureM TypedFExp
convertBody globals = convert . freeVars globals . mapAnn snd

freshFunc :: ClosureM String
freshFunc = do
  (ns, f, c) <- get
  put (ns, f+1, c)
  return $ ns ++ "_f" ++ show f

freshClos :: ClosureM String
freshClos = do
  (ns, f, c) <- get
  put (ns, f, c+1)
  return $ ns ++ "_c" ++ show c

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
extractNames x = trace (show x) $ error "Unsupported"

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

removeRedundantLet :: [TypedExp] -> [TypedExp]
removeRedundantLet es = cataRec alg <$> es
   where 
       alg (Ann attr (Let (In (Ann _ (VarPat n1))) v (In (Ann _ (Var n2))))) | n1 == n2 = v 
       alg x = In x

