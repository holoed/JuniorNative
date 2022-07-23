module Junior.Pretty.Printer where

import Junior.Parser.Primitives ( Prim(U, I, D, B, S, C) )
import Junior.Parser.PAst ( SynExp, SynExpF(IfThenElse, Lit, Var, VarPat, Lam, InfixApp, MkTuple, TuplePat, Let, Defn, ConPat, Match, MatchExp) )
import Junior.Core.Operators ( Operator, Associativity(..), lamOp, minOp )
import Junior.Pretty.Utils ( bracket ) 
import Junior.Utils.RecursionSchemes ( cataRec )
import Text.PrettyPrint.Mainland.Class ()
import Text.PrettyPrint.Mainland ( (<+>),(<+/>), (<|>), char, parens, pretty, folddoc, group, text, line, Doc, parens, sep, spread, nest, align, indent )
import Control.Monad.RWS.Lazy
    ( (<>),
      evalRWS,
      RWS,
      MonadWriter(tell, listen),
      MonadReader(ask, local) )
import Prelude hiding (Left, Right, (<>), pi)
import Junior.Utils.Annotations ( unwrap )
import Data.List (intersperse)

type PrettyM = RWS Int [Operator] ()

isop :: String -> Bool
isop x = x == "++" || x == "+" || x == "*" || x == ":" || x == "!!" || x == ">=>" || x == ">>="

alg :: SynExpF (PrettyM Doc) -> PrettyM Doc
alg (Lit (I v)) =
   return $ text (show v)
alg (Lit (D v)) =
   return $ text (show v)
alg (Lit (B b)) =
   return $ text (show b)
alg (Lit (S s)) =
   return $ text "\"" <> text s <> text "\""
alg (Lit (C c)) =
   return $ text "'" <> text [c] <> text "'"
alg (Lit U) =
   return $ text "()"
alg (ConPat name es) = do
   es' <- sequence es
   return $ (text name) <> (text " ") <> (folddoc (<>) $ intersperse (text " ") es')
alg (VarPat x) =
   return $ if isop x
   then parens (text x)
   else text x
alg (Var x) =
   return $ if isop x
   then parens (text x)
   else text x
alg (Lam ns e) = do
  ns' <- sequence ns
  e' <- e
  _ <- tell [lamOp]
  return $ align $ char '\\' <> sep ns' <+> text "->" <+/> group e'
alg (InfixApp op@(opName, _, _) e1 e2) = do
    (e1', l) <- listen e1
    (e2', r) <- listen e2
    _ <- tell [op]
    let opTxt = if opName == " " then opName else " " ++ opName ++ " "
    return (bracket Left op l e1' <> text opTxt <> bracket Right op r e2')
alg (MkTuple es) = do
  es' <- sequence es
  return $ parens $ folddoc (<>) $ intersperse (text ", ") es'
alg (TuplePat es) = do
  es' <- sequence es
  return $ parens $ folddoc (<>) $ intersperse (text ", ") es'
alg (Let [n] v b) = do
  n' <- n
  v' <- local (const 4) v
  b' <- b
  _ <- tell [minOp]
  level <- ask
  return $ align $ sep [nest level $ text "let" <+> n' <+> char '=' <+> group (nest level v') <+> text "in", group b']
alg (Let ns v b) = do
  ns' <- sequence ns
  let n:xs = ns'
  v' <- local (const 4) v
  b' <- b
  _ <- tell [minOp]
  level <- ask
  return $ align $ sep [nest level $ text "let" <+> n <+> spread xs <+> char '=' <+> group (nest level v') <+> text "in", group b']
alg (IfThenElse q t f) = do
  q' <- q
  t' <- t
  f' <- f
  _ <- tell [minOp]
  level <- ask
  let ret = group $ nest level $ sep [text "if" <+> q', text "then" <+>  group t', text "else" <+> group f']
  return (ret <|> (line <> indent level ret))
alg (Match e1 es) = do
  e1' <- e1
  es' <- sequence es
  return $ (text "match") <+> e1' <+> (text "with") <+> group (folddoc (<>) $ intersperse (text " | ") es')
alg (MatchExp n e) = do
  n' <- n
  e' <- e
  _ <- tell [lamOp]
  return $ align $ n' <+> text "->" <+/> group e'
alg (Defn _ [n] v) = do
  n' <- n
  v' <- local (const 4) v
  _ <- tell [minOp]
  return $ align $ text "let" <+> n' <+> char '=' <+> (v' <|> (line <> group (indent 4 v')))
alg (Defn _ ns v) = do
  ns' <- sequence ns
  let n:xs = ns'
  v' <- local (const 4) v
  _ <- tell [minOp]
  return $ align $ text "let" <+> n <+> spread xs <+> char '=' <+> (v' <|> (line <> group (indent 4 v')))
alg _ = error "Undefined"

prettyDoc :: SynExp -> Doc
prettyDoc = fst . (\e -> evalRWS e 0 ()) . cataRec alg . unwrap

prettyPrint :: SynExp -> String
prettyPrint = pretty 50 . prettyDoc



