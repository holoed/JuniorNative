module PrettyPrinter where

import Ast
import Fixpoint
import RecursionSchemes
import Text.PrettyPrint

pretty :: Exp -> String
pretty = render . (cataRec alg)
  where alg (Var x) = text x
        alg (Lam n e) = char '\\' <> text n <+> text "->" <+> e
