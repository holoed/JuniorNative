module ClosureConversion where

import Ast
import ClosedAst
import CoProduct
import RecursionSchemes
import Monads
import Fixpoint
import Data.Set

type CloseM a = ReaderState (Set String) Int a

alg :: ExpF (CloseM ClosedExp) -> CloseM ClosedExp
alg x = fmap (\v -> In (Inl v)) (traverse id x)

convert :: Exp -> Either String ClosedExp
convert e = eval (cataRec alg e) (fromList []) 0
