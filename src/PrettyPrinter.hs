module PrettyPrinter where

import Ast
import RecursionSchemes
import Data.List
import Text.PrettyPrint
import Control.Monad.Reader
import Prelude hiding ((<>))

alg :: ExpF (Reader [String] Doc) -> Reader [String] Doc
alg (Lit (I v)) =
   return $ text (show v)
alg (Lit (B b)) =
   return $ text (show b)
alg (Lit (S s)) =
   return $ text s 
alg (Var x) =
   return $ text x
alg (Lam n e) = do
  e' <- e
  return $ char '\\' <> text n <+> text "->" <+> e'
alg (App e1 e2) = do
  e1' <- e1
  e2' <- e2
  return $ (e1' <+> e2')
alg (InfixApp op e1 e2) = do
    e1' <- e1
    e2' <- e2
    return $ (e1' <+> e2')
alg (MkTuple es) = do
  es' <- sequence es
  return $ parens $ hcat $ intersperse (text ", ") es'
alg (Let n v b) = do
  v' <- v
  b' <- b
  return $ text "let" <+> text n <+> char '=' <+> v' <+> text "in" <+> b'
alg (IfThenElse q t f) = do
  q' <- q
  t' <- t
  f' <- f
  return $ text "if" <+> q' <+> text "then" <+> t' <+> text "else" <+> f'

pretty :: Exp -> String
pretty = render . (\e -> runReader (cataRec alg e) [])
