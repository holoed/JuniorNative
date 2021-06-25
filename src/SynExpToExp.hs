{-# LANGUAGE TupleSections #-}
module SynExpToExp where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, andOp, orOp, gtOp, ltOp )
import RecursionSchemes ( cataRec )

mkLoc :: PAst.Loc -> Ast.Loc
mkLoc (PAst.Loc x y z) = Ast.Loc x y z

toSynLoc :: Ast.Loc -> PAst.Loc
toSynLoc (Ast.Loc x y z) = PAst.Loc x y z

toExp :: PAst.SynExp -> Ast.Exp
toExp = cataRec alg
    where alg (Ann (Just l) (PAst.Lit x)) = Ast.lit (mkLoc l) x
          alg (Ann (Just l) (PAst.Var s)) = Ast.var (mkLoc l) s
          alg (Ann (Just l) (PAst.VarPat s)) = Ast.varPat (mkLoc l) s
          alg (Ann (Just l) (PAst.MkTuple es)) = Ast.mkTuple (mkLoc l) es
          alg (Ann (Just l) (PAst.TuplePat es)) = Ast.tuplePat (mkLoc l) es
          alg (Ann Nothing (PAst.App e1 e2)) = Ast.app e1 e2
          alg (Ann _ (PAst.InfixApp (" ",_,_) e1 e2)) = Ast.app e1 e2
          alg (Ann (Just l) (PAst.InfixApp (op, _, _) e1 e2)) =
              Ast.app (Ast.app (Ast.var (mkLoc l) op) e1) e2
          alg (Ann (Just l) (PAst.Lam ss e)) =
              foldr (Ast.lam (mkLoc l)) e ss
          alg (Ann (Just l) (PAst.Let [s] e1 e2)) =
              Ast.leT (mkLoc l) s e1 e2
          alg (Ann (Just l) (PAst.Let (s:ss) e1 e2)) =
              Ast.leT (mkLoc l) s (foldr (Ast.lam (mkLoc l)) e1 ss) e2
          alg (Ann (Just l) (PAst.IfThenElse p e1 e2)) = Ast.ifThenElse (mkLoc l) p e1 e2
          alg x = error ("toExp error: " ++ show x)

fromExp :: Ast.Exp -> PAst.SynExp
fromExp = cataRec alg
    where alg (Ann (Just l) (Ast.Lit x)) = PAst.lit (toSynLoc l) x
          alg (Ann (Just l) (Ast.Var s)) = PAst.var (toSynLoc l) s
          alg (Ann (Just l) (Ast.VarPat s)) = PAst.varPat (toSynLoc l) s
          alg (Ann (Just l) (Ast.MkTuple es)) = PAst.mkTuple (toSynLoc l) es
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "*"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc mulOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc divOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "+"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc plusOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "-"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc subOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "=="))) e1))) e2)) = PAst.infixApp PAst.zeroLoc eqeqOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "&&"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc andOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "||"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc orOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc gtOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc ltOp e1 e2
          alg (Ann Nothing (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "++"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc plusplusOp e1 e2
          alg (Ann Nothing (Ast.App e1 e2)) = PAst.infixApp PAst.zeroLoc juxtaOp e1 e2
          alg (Ann (Just l) (Ast.Lam s e)) = PAst.lam (toSynLoc l) [s] e
          alg (Ann (Just l) (Ast.Let s e1 e2)) = PAst.leT (toSynLoc l) [s] e1 e2
          alg (Ann (Just l) (Ast.IfThenElse p e1 e2)) = PAst.ifThenElse (toSynLoc l) p e1 e2
          alg x = error ("fromExp error: " ++ show x)