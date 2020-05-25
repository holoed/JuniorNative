module PrettyPrinter where

import Primitives
import PAst
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
  where other Postfix Left  = True
        other Prefix  Right = True
        other (Infix Left) Left = pi == po && fo == Infix Left
        other (Infix Right) Right = pi == po && fo == Infix Right
        other _ NonAssoc = fi == fo
        other _ _ = False
       
bracket :: Associativity -> Operator -> [Operator] -> Doc -> Doc
bracket _ _ [] doc = doc
bracket side outer (inner:_) doc =
  if noparens inner outer side then doc else parenthesize doc

alg :: SynExpF (Writer [Operator] Doc) -> Writer [Operator] Doc
alg (Lit (I v)) =
   return $ text (show v)
alg (Lit (B b)) =
   return $ text (show b)
alg (Lit (S s)) =
   return $ text s 
alg (Lit U) =
   return $ text "()" 
alg (Var x) =
   return $ text x
alg (Lam [n] e) = do
  e' <- e
  _ <- tell [lamOp]
  return $ char '\\' <> text n <+> text "->" <+> e'
alg (InfixApp op@(opName, _, _) e1 e2) = do
    (e1', l) <- listen e1
    (e2', r) <- listen e2
    _ <- tell [op]
    let opTxt = if opName == " " then opName else " " ++ opName ++ " "
    return $ ((bracket Left op l e1') <> text opTxt <> (bracket Right op r e2'))
alg (MkTuple es) = do
  es' <- sequence es
  return $ parens $ hcat $ intersperse (text ", ") es'
alg (Let [n] v b) = do
  v' <- v
  b' <- b
  _ <- tell [minOp]
  if (b' == text n) then return $ text "let" <+> text n <+> char '=' <+> v'
  else return $ text "let" <+> text n <+> char '=' <+> v' <+> text "in" <+> b'
alg (Let (n:xs) v b) = do
  v' <- v
  b' <- b
  _ <- tell [minOp]
  return $ text "let" <+> text n <+> foldr (\x acc -> acc <+> text x) (text $ head xs) (tail xs) <+> char '=' <+> v' <+> text "in" <+> b'
alg (IfThenElse q t f) = do
  q' <- q
  t' <- t
  f' <- f
  _ <- tell [minOp]
  return $ text "if" <+> q' <+> text "then" <+> t' <+> text "else" <+> f'

pretty :: SynExp -> String
pretty = render . (\e -> fst $ runWriter (cataRec alg e))
