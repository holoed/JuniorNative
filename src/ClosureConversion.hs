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
alg (Lam n e) = fmap (mkClosure (mkEnv []) . cLam n) e
alg x = fmap (In . Inl) (traverse id x)

convert :: Exp -> Either String ClosedExp
convert e = eval (cataRec alg e) (fromList []) 0
