{-# LANGUAGE TupleSections #-}
module SynExpToExp where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, andOp, orOp, gtOp, ltOp, consOp )
import RecursionSchemes ( cataRec )
import Location (zeroLoc)

toExp :: PAst.SynExp -> Ast.Exp
toExp = cataRec alg
    where alg (Ann (Just l) (PAst.Lit x)) = Ast.lit l x
          alg (Ann (Just l) (PAst.Var s)) = Ast.var l s
          alg (Ann (Just l) (PAst.VarPat s)) = Ast.varPat l s
          alg (Ann (Just l) (PAst.MkTuple es)) = Ast.mkTuple l es
          alg (Ann (Just l) (PAst.TuplePat es)) = Ast.tuplePat l es
          alg (Ann Nothing (PAst.App e1 e2)) = Ast.app e1 e2
          alg (Ann _ (PAst.InfixApp (" ",_,_) e1 e2)) = Ast.app e1 e2
          alg (Ann (Just l) (PAst.InfixApp (op, _, _) e1 e2)) =
              Ast.app (Ast.app (Ast.var l op) e1) e2
          alg (Ann (Just l) (PAst.Lam ss e)) =
              foldr (Ast.lam l) e ss
          alg (Ann (Just l) (PAst.Let [s] e1 e2)) =
              Ast.leT l s e1 e2
          alg (Ann (Just l) (PAst.Let (s:ss) e1 e2)) =
              Ast.leT l s (foldr (Ast.lam l) e1 ss) e2
          alg (Ann (Just l) (PAst.IfThenElse p e1 e2)) = Ast.ifThenElse l p e1 e2
          alg x = error ("toExp error: " ++ show x)


fromExp :: Ast.Exp -> PAst.SynExp
fromExp = compressLets . compressLambdas . cataRec alg
    where alg (Ann (Just l) (Ast.Lit x)) = PAst.lit l x
          alg (Ann (Just l) (Ast.Var s)) = PAst.var l s
          alg (Ann (Just l) (Ast.VarPat s)) = PAst.varPat l s
          alg (Ann (Just l) (Ast.TuplePat es)) = PAst.tuplePat l es
          alg (Ann (Just l) (Ast.MkTuple es)) = PAst.mkTuple l es
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "*"))) e1))) e2)) = PAst.infixApp zeroLoc mulOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/"))) e1))) e2)) = PAst.infixApp zeroLoc divOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "+"))) e1))) e2)) = PAst.infixApp zeroLoc plusOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "-"))) e1))) e2)) = PAst.infixApp zeroLoc subOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "=="))) e1))) e2)) = PAst.infixApp zeroLoc eqeqOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "&&"))) e1))) e2)) = PAst.infixApp zeroLoc andOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "||"))) e1))) e2)) = PAst.infixApp zeroLoc orOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">"))) e1))) e2)) = PAst.infixApp zeroLoc gtOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<"))) e1))) e2)) = PAst.infixApp zeroLoc ltOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "++"))) e1))) e2)) = PAst.infixApp zeroLoc plusplusOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ":"))) e1))) e2)) = PAst.infixApp zeroLoc consOp e1 e2
          alg (Ann Nothing (Ast.App e1 e2)) = PAst.infixApp zeroLoc juxtaOp e1 e2
          alg (Ann (Just l) (Ast.Lam s e)) = PAst.lam l [s] e
          alg (Ann (Just l) (Ast.Let s e1 e2)) = PAst.leT l [s] e1 e2
          alg (Ann (Just l) (Ast.IfThenElse p e1 e2)) = PAst.ifThenElse l p e1 e2
          alg x = error ("fromExp error: " ++ show x)

compressLambdas :: PAst.SynExp -> PAst.SynExp
compressLambdas = cataRec alg
 where alg (Ann (Just l) (PAst.Lam n1 (In (Ann _ (PAst.Lam n2 v))))) = 
             PAst.lam l (n1 ++ n2) v
       alg e = In e

compressLets :: PAst.SynExp -> PAst.SynExp
compressLets = cataRec alg
 where alg (Ann (Just l) (PAst.Let n1 (In (Ann _ (PAst.Lam n2 v))) b)) = 
             PAst.leT l (n1 ++ n2) v b
       alg e = In e
