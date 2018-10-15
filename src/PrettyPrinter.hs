module PrettyPrinter where

import Ast
import Fixpoint
import RecursionSchemes
import Text.PrettyPrint
import Control.Monad.Reader

alg (Lit (I v)) =
   return $ text (show v)                                        
alg (Lit (B b)) =
   return $ text (show b)                                        
alg (Var x) =
   return $ text x                                                       
alg (Lam n e) = do
  e' <- e
  return $ parens $ char '\\' <> text n <+> text "->" <+> e'            
alg (App e1 e2) = do
  e1' <- e1
  e2' <- e2
  return $ parens $ e1' <+> e2'

pretty :: Exp -> String
pretty = render . (\e -> runReader (cataRec alg e) 0)
