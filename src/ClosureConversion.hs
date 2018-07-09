module ClosureConversion where

import Ast
import ClosedAst

type CloseM = ReaderState (Set String) Int

alg :: ExpF -> CloseM (Fix[ClosedExpF])
alg = undefined


convert :: Fix[ExpF] -> Fix[ClosedExpF]
convert = undefined
