module ClosureConversion where

import Ast
import ClosedAst
import CoProduct
import RecursionSchemes
import Monads
import Fixpoint
import Data.Set

type CloseM = ReaderState (Set String) Int

alg :: ExpF (CloseM ClosedExp) -> CloseM ClosedExp
alg (Lam n e) = do
     e' <- local (insert n) e
     ctx <- ask
     let newEnv = mkEnv (fmap cVar (toList ctx))
     return $ (mkClosure newEnv . cLam n) e'
alg x = fmap (In . Inl) (traverse id x)

convert :: Exp -> Either String ClosedExp
convert e = eval (cataRec alg e) (fromList []) 0
