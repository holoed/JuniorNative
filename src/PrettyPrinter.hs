module PrettyPrinter where

import Ast
import Fixpoint
import RecursionSchemes
import Text.PrettyPrint

pretty :: Exp -> String
pretty = render . (cataRec alg)
  where alg (Var x) = text x
        alg (Lam n e) = parens $ char '\\' <> text n <+> text "->" <+> e
        alg (App e1 e2) = parens $ e1 <+> e2
