module SynExpToExp where

import qualified Ast
import qualified PAst
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, gtOp, ltOp )
import RecursionSchemes ( cataRec )

toExp :: PAst.SynExp -> Ast.Exp
toExp = cataRec alg
    where alg (PAst.Lit x) = Ast.lit x
          alg (PAst.Var s) = Ast.var s
          alg (PAst.MkTuple es) = Ast.mkTuple es
          alg (PAst.App e1 e2) = Ast.app e1 e2
          alg (PAst.InfixApp (" ",_,_) e1 e2) = Ast.app e1 e2
          alg (PAst.InfixApp (op, _, _) e1 e2) = Ast.app (Ast.app (Ast.var op) e1) e2
          alg (PAst.Lam ss e) = foldr Ast.lam e ss
          alg (PAst.Let [s] e1 e2) = Ast.leT s e1 e2
          alg (PAst.Let (s:ss) e1 e2) = Ast.leT s (foldr Ast.lam e1 ss) e2
          alg (PAst.IfThenElse p e1 e2) = Ast.ifThenElse p e1 e2
          alg _ = error "Undefined"

fromExp :: Ast.Exp -> PAst.SynExp
fromExp = cataRec alg
    where alg (Ast.Lit x) = PAst.lit PAst.zeroLoc  x
          alg (Ast.Var s) = PAst.var s
          alg (Ast.MkTuple es) = PAst.mkTuple es
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "*")) e1)) e2) = PAst.infixApp mulOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "/")) e1)) e2) = PAst.infixApp divOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "+")) e1)) e2) = PAst.infixApp plusOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "-")) e1)) e2) = PAst.infixApp subOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "==")) e1)) e2) = PAst.infixApp eqeqOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var ">")) e1)) e2) = PAst.infixApp gtOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "<")) e1)) e2) = PAst.infixApp ltOp e1 e2
          alg (Ast.App (In (PAst.InfixApp (" ", _, _) (In (PAst.Var "++")) e1)) e2) = PAst.infixApp plusplusOp e1 e2
          alg (Ast.App e1 e2) = PAst.infixApp juxtaOp e1 e2
          alg (Ast.Lam s e) = PAst.lam [s] e
          alg (Ast.Let s e1 e2) = PAst.leT [s] e1 e2
          alg (Ast.IfThenElse p e1 e2) = PAst.ifThenElse PAst.zeroLoc  p e1 e2
