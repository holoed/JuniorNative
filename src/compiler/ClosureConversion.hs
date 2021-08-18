module ClosureConversion where

import Location (zeroLoc)
import Annotations ( Ann(Ann), mapAnn )
import Fixpoint ( Fix(In) )
import Ast ( ExpF(Var, Lit, VarPat, MkTuple, TuplePat, Lam, App, SetEnv, GetEnv, Var, Let, MkClosure, Defn, ClosureRef, IfThenElse) )
import Data.Set ( Set, toList, member, fromList )
import Control.Monad.RWS.Lazy ( RWS, asks, runRWS )
import Control.Monad.Writer ( MonadWriter(tell) )
import Control.Monad.State ( MonadState(put, get) )
import RecursionSchemes (cataRec)
import FreeVariables (FreeVarsExp, freeVars)
import Types (Qual, Type)
import TypedAst (TypedExp)

type TypedFExp = FreeVarsExp (Qual Type)
type ClosureM = RWS (Set String) [TypedFExp] (Int, Int)

convertProg :: [TypedExp] -> [TypedExp]
convertProg defs = mapAnn (\(qt, _) -> (Just zeroLoc, qt)) <$> origDefs ++ reverse newDefs
  where
    globals = fromList . map (\(In (Ann _ (Defn _ (In (Ann _ (VarPat name))) _))) -> name) $ defs
    newDefsM = sequence $ convert . freeVars globals . mapAnn snd <$> defs
    (origDefs, _, newDefs) = runRWS newDefsM globals (0, 0)

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

setEnv :: (Qual Type, Set String) -> String -> String -> TypedFExp -> TypedFExp
setEnv attr closName name x = In (Ann attr (SetEnv name (In (Ann attr (Var closName)))
                                                        (In (Ann attr (Var name))) x))

convert :: TypedFExp -> ClosureM TypedFExp
convert = cataRec alg
    where
     alg (Ann attr (Lit x)) = return $ In (Ann attr (Lit x))
     alg (Ann attr (Var n)) = return $ In (Ann attr (Var n))
     alg (Ann attr (VarPat n)) = return $ In (Ann attr (VarPat n))
     alg (Ann attr (MkTuple es)) =
         do es' <- sequence es
            return $ In (Ann attr (MkTuple es'))
     alg (Ann attr (TuplePat es)) =
         do es' <- sequence es
            return $ In (Ann attr (TuplePat es'))
     alg (Ann attr (Let n v b)) = do
            n' <- n
            v' <- v
            In . Ann attr . Let n' v' <$> b
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
            let envBindings = foldr (setEnv attr closName) (In (Ann attr (Var closName))) (toList freeVars')
            return $ In (Ann attr (Let (In (Ann attr (Var closName)))
                                       (In (Ann attr (MkClosure name)))
                                       envBindings))

     alg (Ann attr (App e1 e2)) = do
           e1' <- e1
           let (In (Ann _ (VarPat name))) = e1'
           e2' <- e2
           isGlobal <- asks (member name)
           if isGlobal
           then return $ In (Ann attr (App (In (Ann attr (Var name))) e2'))
           else return $ callClosure attr (In (Ann attr (Var name))) e2'
     alg (Ann attr (MkClosure name)) = return $ In (Ann attr (MkClosure name))
     alg (Ann attr (ClosureRef clos)) = In . Ann attr . ClosureRef <$> clos
     alg (Ann attr (SetEnv name clos binding body)) = do
        clos' <- clos
        binding' <- binding
        In . Ann attr . SetEnv name clos' binding' <$> body
     alg (Ann attr (GetEnv name clos)) = do
        In . Ann attr . GetEnv name <$> clos
     alg (Ann attr (IfThenElse e1 e2 e3)) = do
        e1' <- e1
        e2' <- e2
        In . Ann attr . IfThenElse e1' e2' <$> e3
     alg (Ann _ Defn {}) = undefined

subst :: Set String -> TypedFExp -> TypedFExp -> TypedFExp
subst = undefined

callClosure ::  (Qual Type, Set String) -> TypedFExp -> TypedFExp -> TypedFExp
callClosure attr closure args =
    In (Ann attr (App e1 e2))
    where e1 = In (Ann attr (ClosureRef closure))
          e2 = args
