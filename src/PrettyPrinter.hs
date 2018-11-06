module PrettyPrinter where

import CoProduct
import Ast
import ClosedAst
import RecursionSchemes
import Data.List
import Text.PrettyPrint
import Control.Monad.Reader


parensIf :: [String] -> Doc -> Doc
parensIf ("AppL":"Lam":_) d = parens d
parensIf ("AppL":"Let":_) d = parens d
parensIf ("App":"AppR":_) d = parens d
parensIf ("Lam":"AppL":_) d = parens d
parensIf ("Let":"AppL":_) d = parens d
parensIf ("Lam":"AppR":_) d = parens d
parensIf ("Let":"AppR":_) d = parens d
parensIf ("If" :"AppL":_) d = parens d
parensIf ("If" :"AppR":_) d = parens d
parensIf ("If" :"Then":_) d = parens d
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
alg (MkTuple es) = do
  es' <- sequence es
  return $ parens $ hcat $ intersperse (text ", ") es'
alg (Let n v b) = do
  p <- ask
  v' <- v
  b' <- local ("LetB" :) b
  return $ parensIf ("Let":p) $ text "let" <+> text n <+> char '=' <+> v' <+> text "in" <+> b'
alg (IfThenElse q t f) = do
  p <- ask
  q' <- q
  t' <- local ("Then":) t
  f' <- local ("Else":) f
  return $ parensIf ("If":p) $ text "if" <+> q' <+> text "then" <+> t' <+> text "else" <+> f'

pretty :: Exp -> String
pretty = render . (\e -> runReader (cataRec alg e) [])

algC :: ClosureF (Reader [String] Doc) -> Reader [String] Doc
algC (LookupEnv s n) = return $ parens $ text "lookupEnv" <+> quotes (text s) <+> text (show n)
algC (MakeEnv s env) = do
  env' <- sequence env
  return $ text "mkEnv" <+> quotes (text s) <+> text "[" <> hcat (intersperse (text ", ") env') <> text "]"
algC (MakeClosure e1 e2) = do
  e1' <- e1
  e2' <- e2
  return $ text "mkClosure" <+> parens e1' <+> parens e2'

prettyClosed :: ClosedExp -> String
prettyClosed = render . (\e ->runReader (cataRec (liftAlg alg algC) e) [])
