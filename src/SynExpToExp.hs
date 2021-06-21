module SynExpToExp where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) ) 
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, gtOp, ltOp )
import RecursionSchemes ( cataRec )

toExp :: PAst.SynExp -> Ast.Exp
toExp = cataRec alg
    where alg (Ann _ (PAst.Lit x)) = Ast.lit x
          alg (Ann _ (PAst.Var s)) = Ast.var s
          alg (Ann _ (PAst.MkTuple es)) = Ast.mkTuple es
          alg (Ann _ (PAst.App e1 e2)) = Ast.app e1 e2
          alg (Ann _ (PAst.InfixApp (" ",_,_) e1 e2)) = Ast.app e1 e2
          alg (Ann _ (PAst.InfixApp (op, _, _) e1 e2)) = Ast.app (Ast.app (Ast.var op) e1) e2
          alg (Ann _ (PAst.Lam ss e)) = foldr Ast.lam e ss
          alg (Ann _ (PAst.Let [s] e1 e2)) = Ast.leT s e1 e2
          alg (Ann _ (PAst.Let (s:ss) e1 e2)) = Ast.leT s (foldr Ast.lam e1 ss) e2
          alg (Ann _ (PAst.IfThenElse p e1 e2)) = Ast.ifThenElse p e1 e2
          alg _ = error "Undefined"

fromExp :: Ast.Exp -> PAst.SynExp
fromExp = cataRec alg
    where alg (Ast.Lit x) = PAst.lit PAst.zeroLoc x
          alg (Ast.Var s) = PAst.var PAst.zeroLoc s
          alg (Ast.MkTuple es) = PAst.mkTuple PAst.zeroLoc  es
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "*"))) e1))) e2) = PAst.infixApp PAst.zeroLoc mulOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/"))) e1))) e2) = PAst.infixApp PAst.zeroLoc divOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "+"))) e1))) e2) = PAst.infixApp PAst.zeroLoc plusOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "-"))) e1))) e2) = PAst.infixApp PAst.zeroLoc subOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "=="))) e1))) e2) = PAst.infixApp PAst.zeroLoc eqeqOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">"))) e1))) e2) = PAst.infixApp PAst.zeroLoc gtOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<"))) e1))) e2) = PAst.infixApp PAst.zeroLoc ltOp e1 e2
          alg (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "++"))) e1))) e2) = PAst.infixApp PAst.zeroLoc plusplusOp e1 e2
          alg (Ast.App e1 e2) = PAst.infixApp PAst.zeroLoc juxtaOp e1 e2
          alg (Ast.Lam s e) = PAst.lam PAst.zeroLoc [(PAst.zeroLoc, s)] e
          alg (Ast.Let s e1 e2) = PAst.leT PAst.zeroLoc [(PAst.zeroLoc, s)] e1 e2
          alg (Ast.IfThenElse p e1 e2) = PAst.ifThenElse PAst.zeroLoc  p e1 e2
