module SynExpToExp where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, gtOp, ltOp )
import RecursionSchemes ( cataRec )
import Data.Map ((!))

mkLoc :: PAst.Loc -> Ast.Loc
mkLoc (PAst.Loc x y z) = Ast.Loc x y z

toSynLoc :: Ast.Loc -> PAst.Loc
toSynLoc (Ast.Loc x y z) = PAst.Loc x y z

toExp :: PAst.SynExp -> Ast.Exp
toExp = cataRec alg
    where alg (Ann (PAst.LitLoc l) (PAst.Lit x)) = Ast.lit (mkLoc l) x
          alg (Ann (PAst.VarLoc l) (PAst.Var s)) = Ast.var (mkLoc l) s
          alg (Ann (PAst.TupleLoc l) (PAst.MkTuple es)) = Ast.mkTuple (mkLoc l) es
          alg (Ann PAst.AppLoc (PAst.App e1 e2)) = Ast.app e1 e2
          alg (Ann _ (PAst.InfixApp (" ",_,_) e1 e2)) = Ast.app e1 e2
          alg (Ann (PAst.InfixAppLoc l) (PAst.InfixApp (op, _, _) e1 e2)) = Ast.app (Ast.app (Ast.var (mkLoc l) op) e1) e2
          alg (Ann (PAst.LamLoc l l') (PAst.Lam ss e)) = foldr (Ast.lam (mkLoc l)) e ((\s -> (s, mkLoc $ l'!s)) <$> ss)
          alg (Ann (PAst.LetLoc l l') (PAst.Let [s] e1 e2)) =
              Ast.leT (mkLoc l) (s, mkLoc $ l'!s) e1 e2
          alg (Ann (PAst.LetLoc l l') (PAst.Let (s:ss) e1 e2)) =
              Ast.leT (mkLoc l) (s, mkLoc $ l'!s) (foldr (Ast.lam (mkLoc l)) e1 ((\s -> (s, mkLoc $ l'!s)) <$> ss)) e2
          alg (Ann (PAst.IfThenElseLoc l) (PAst.IfThenElse p e1 e2)) = Ast.ifThenElse (mkLoc l) p e1 e2
          alg _ = error "Undefined"

fromExp :: Ast.Exp -> PAst.SynExp
fromExp = cataRec alg
    where alg (Ann (Ast.LitLoc l) (Ast.Lit x)) = PAst.lit (toSynLoc l) x
          alg (Ann (Ast.VarLoc l) (Ast.Var s)) = PAst.var (toSynLoc l) s
          alg (Ann (Ast.TupleLoc l) (Ast.MkTuple es)) = PAst.mkTuple (toSynLoc l) es
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "*"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc mulOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc divOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "+"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc plusOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "-"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc subOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "=="))) e1))) e2)) = PAst.infixApp PAst.zeroLoc eqeqOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc gtOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc ltOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "++"))) e1))) e2)) = PAst.infixApp PAst.zeroLoc plusplusOp e1 e2
          alg (Ann Ast.AppLoc (Ast.App e1 e2)) = PAst.infixApp PAst.zeroLoc juxtaOp e1 e2
          alg (Ann (Ast.LamLoc l l') (Ast.Lam s e)) = PAst.lam (toSynLoc l) [(toSynLoc l', s)] e
          alg (Ann (Ast.LetLoc l l') (Ast.Let s e1 e2)) = PAst.leT (toSynLoc l) [(toSynLoc l', s)] e1 e2
          alg (Ann (Ast.IfThenElseLoc l) (Ast.IfThenElse p e1 e2)) = PAst.ifThenElse (toSynLoc l) p e1 e2
          alg _ = error "Undefined"