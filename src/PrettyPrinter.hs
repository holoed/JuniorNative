module PrettyPrinter where

import Ast
import Fixpoint
import RecursionSchemes
import Text.PrettyPrint
import Control.Monad.Reader


parensIf :: [String] -> Doc -> Doc
parensIf ("AppL":"Lam":_) d = parens d
parensIf ("App":"AppR":_) d = parens d
parensIf ("Lam":"AppL":_) d = parens d
parensIf ("Lam":"AppR":_) d = parens d
parensIf _ d = d

alg :: ExpF (Reader [String] Doc) -> Reader [String] Doc 
alg (Lit (I v)) =
   return $ text (show v)                                        
alg (Lit (B b)) =
   return $ text (show b)                                        
alg (Var x) =
   return $ text x                                                       
alg (Lam n e) = do
  p  <- ask
  e' <- local ("Lam":) e
  return $ parensIf ("Lam" :p) $ char '\\' <> text n <+> text "->" <+> e'            
alg (App e1 e2) = do
  p <- ask
  e1' <- local ("AppL" :) e1
  e2' <- local ("AppR" :) e2
  return $ parensIf ("App":p) (e1' <+> e2')

pretty :: Exp -> String
pretty = render . (\e -> runReader (cataRec alg e) [])
