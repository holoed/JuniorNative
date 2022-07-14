module DesugarQuotation where

import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Annotations (Ann(Ann))
import Ast (ExpF(Var, App, Lit, AppClosure, Let, VarPat))
import Primitives (Prim(S))

desugar :: [TypedExp] -> [TypedExp]
desugar es = (cataRec alg <$> es)
    where
        alg :: TypedExpF (TypedExp) -> TypedExp
        alg (Ann attr (Let (In (Ann _ (VarPat n1))) v (In (Ann _ (Var n2))))) | n1 == n2 = v 
        alg (Ann attr1 (AppClosure e1@(In (Ann _ (Var "quote"))) (In (Ann attr3 (Var f))))) = 
            In (Ann attr1 (AppClosure e1 (In (Ann attr3 (Lit (S f))))))
        alg x = In x


