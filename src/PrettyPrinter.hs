module PrettyPrinter where

import Ast
import Operators
import RecursionSchemes
import Data.List
import Text.PrettyPrint
import Control.Monad.Writer hiding ((<>))
import Prelude hiding (Left, Right, (<>))

parenthesize :: Doc -> Doc
parenthesize d = text "(" <> d <> text ")"

noparens ::Operator -> Operator -> Associativity -> Bool
noparens (_, pi, fi) (_, po, fo) side = pi > po || other fi side
  where other Postfix Left = True

unwrapOp ::[Operator] -> Operator
unwrapOp [] = maxOp
unwrapOp [op] = op

bracket :: Associativity -> Operator -> [Operator] -> Doc -> Doc
bracket side outer inner doc =
  if noparens (unwrapOp inner) outer side then doc else parenthesize doc

alg :: ExpF (Writer [Operator] Doc) -> Writer [Operator] Doc
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
alg (InfixApp op@(opName, _, _) e1 e2) = do
    (e1', l) <- listen e1
    (e2', r) <- listen e2
    return $ ((bracket Left op l e1') <> text opName <> (bracket Right op r e2'))
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
pretty = render . (\e -> fst $ runWriter (cataRec alg e))
