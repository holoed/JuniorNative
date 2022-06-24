{-# LANGUAGE TupleSections #-}
module SynExpToExp where

import qualified Ast
import qualified PAst
import Annotations ( Ann(Ann) )
import Fixpoint ( Fix(In) )
import Operators ( juxtaOp, mulOp, divOp, plusOp, plusplusOp, subOp, eqeqOp, gteqOp, lteqOp, andOp, orOp, gtOp, ltOp, consOp, ltStarGtOp, ltGtOp, noteqOp, gtEqGtOp, gtGtEqOp, ltDollarGtOp )
import RecursionSchemes ( cataRec )
import Location (Loc, zeroLoc)
import Data.Maybe ( fromMaybe )
import TypesPrinter () 
import Control.Applicative (Applicative(liftA2))

getLoc :: Maybe Loc -> Loc
getLoc = fromMaybe zeroLoc

toExp :: PAst.SynExp -> Maybe Ast.Exp
toExp = cataRec alg
    where alg (Ann (Just l) (PAst.Defn qt [s] e1)) = 
               pure (Ast.defn l qt) <*> s <*> e1
          alg (Ann (Just l) (PAst.Defn qt (s:ss) e1)) =
               pure (Ast.defn l qt) <*> s <*> (foldr (liftA2 (Ast.lam l)) e1 ss) 
          alg (Ann (Just l) (PAst.Lit x)) = pure $ Ast.lit l x
          alg (Ann (Just l) (PAst.Var s)) = pure $ Ast.var l s
          alg (Ann (Just l) (PAst.VarPat s)) = pure $ Ast.varPat l s
          alg (Ann (Just l) (PAst.LitPat x)) = pure $ Ast.litPat l x
          alg (Ann (Just l) (PAst.MkTuple es)) = Ast.mkTuple l <$> (sequence es)
          alg (Ann (Just l) (PAst.TuplePat es)) = Ast.tuplePat l <$> (sequence es)
          alg (Ann (Just l) (PAst.ConPat name es)) = Ast.conPat l name <$> (sequence es)
          alg (Ann Nothing (PAst.App e1 e2)) = pure Ast.app <*> e1 <*> e2
          alg (Ann _ (PAst.InfixApp (" ",_,_) e1 e2)) = pure Ast.app <*> e1 <*> e2
          alg (Ann (Just l) (PAst.InfixApp (op, _, _) e1 e2)) =
              pure (\x y -> Ast.app (Ast.app (Ast.var l op) x) y) <*> e1 <*> e2
          alg (Ann (Just l) (PAst.Lam ss e)) =
              pure (foldr (Ast.lam l)) <*> e <*> (sequence ss)
          alg (Ann (Just l) (PAst.Let [s] e1 e2)) =
              pure (Ast.leT l) <*> s <*> e1 <*> e2
          alg (Ann (Just l) (PAst.Let (s:ss) e1 e2)) =
              pure (Ast.leT l) <*> s <*> (pure (foldr (Ast.lam l)) <*> e1 <*> (sequence ss)) <*> e2
          alg (Ann (Just l) (PAst.IfThenElse p e1 e2)) = pure (Ast.ifThenElse l) <*> p <*> e1 <*> e2
          alg (Ann (Just l) (PAst.Match e es)) = pure (Ast.matcH l) <*> e <*> sequence es
          alg (Ann (Just l) (PAst.MatchExp e1 e2)) = pure (Ast.matchExp l) <*> e1 <*> e2
          alg _ = Nothing


fromExp :: Ast.Exp -> PAst.SynExp
fromExp = compressDefn . compressLets . compressLambdas . cataRec alg
    where alg (Ann (Just l) (Ast.Lit x)) = PAst.lit l x
          alg (Ann (Just l) (Ast.Var s)) = PAst.var l s
          alg (Ann l (Ast.VarPat s)) = PAst.varPat (getLoc l) s
          alg (Ann l (Ast.ConPat name xs)) = PAst.conPat (getLoc l) name xs
          alg (Ann (Just l) (Ast.TuplePat es)) = PAst.tuplePat l es
          alg (Ann (Just l) (Ast.MkTuple es)) = PAst.mkTuple l es
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "*"))) e1))) e2)) = PAst.infixApp zeroLoc mulOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/"))) e1))) e2)) = PAst.infixApp zeroLoc divOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "+"))) e1))) e2)) = PAst.infixApp zeroLoc plusOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "-"))) e1))) e2)) = PAst.infixApp zeroLoc subOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "=="))) e1))) e2)) = PAst.infixApp zeroLoc eqeqOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "/="))) e1))) e2)) = PAst.infixApp zeroLoc noteqOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">="))) e1))) e2)) = PAst.infixApp zeroLoc gteqOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<="))) e1))) e2)) = PAst.infixApp zeroLoc lteqOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "&&"))) e1))) e2)) = PAst.infixApp zeroLoc andOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "||"))) e1))) e2)) = PAst.infixApp zeroLoc orOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">"))) e1))) e2)) = PAst.infixApp zeroLoc gtOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<"))) e1))) e2)) = PAst.infixApp zeroLoc ltOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "++"))) e1))) e2)) = PAst.infixApp zeroLoc plusplusOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ":"))) e1))) e2)) = PAst.infixApp zeroLoc consOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<*>"))) e1))) e2)) = PAst.infixApp zeroLoc ltStarGtOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<$>"))) e1))) e2)) = PAst.infixApp zeroLoc ltDollarGtOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var "<>"))) e1))) e2)) = PAst.infixApp zeroLoc ltGtOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">=>"))) e1))) e2)) = PAst.infixApp zeroLoc gtEqGtOp e1 e2
          alg (Ann _ (Ast.App (In (Ann _ (PAst.InfixApp (" ", _, _) (In (Ann _ (PAst.Var ">>="))) e1))) e2)) = PAst.infixApp zeroLoc gtGtEqOp e1 e2
          alg (Ann _ (Ast.App e1 e2)) = PAst.infixApp zeroLoc juxtaOp e1 e2
          alg (Ann l (Ast.Lam s e)) = PAst.lam (getLoc l) [s] e
          alg (Ann (Just l) (Ast.Let s e1 e2)) = PAst.leT l [s] e1 e2
          alg (Ann (Just l) (Ast.Defn qt s e1)) = PAst.defn l qt [s] e1
          alg (Ann (Just l) (Ast.IfThenElse p e1 e2)) = PAst.ifThenElse l p e1 e2
          alg (Ann (Just l) (Ast.Match e1 e2s)) = PAst.matcH l e1 e2s
          alg (Ann (Just l) (Ast.MatchExp e1 e2)) = PAst.patternMatch l e1 e2
          alg (Ann (Just l) (Ast.MkClosure name)) = 
              PAst.infixApp l juxtaOp (PAst.var l "MkClosure") (PAst.var l name)
          alg (Ann (Just l) (Ast.SetEnv name e1 e2)) =
              PAst.infixApp l juxtaOp (PAst.var l "SetEnv") (PAst.mkTuple l [PAst.var l ("\"" ++ name ++ "\""), e1, e2])
          alg (Ann (Just l) (Ast.GetEnv name e)) = 
              PAst.infixApp l juxtaOp (PAst.var l "GetEnv") (PAst.mkTuple l [PAst.var l ("\"" ++ name ++ "\""), e])
          alg (Ann (Just l) (Ast.AppClosure e1 e2)) =
              PAst.infixApp l juxtaOp (PAst.var l "AppClosure") (PAst.mkTuple l [e1, e2])
          alg x = PAst.var zeroLoc ("# " ++ show x ++ " #")

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

compressDefn :: PAst.SynExp -> PAst.SynExp
compressDefn = cataRec alg
 where alg (Ann (Just l) (PAst.Defn Nothing n1 (In (Ann _ (PAst.Lam n2 v))))) =
             PAst.defn l Nothing (n1 ++ n2) v
       alg e = In e
