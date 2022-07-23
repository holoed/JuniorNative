module Junior.Compiler.DesugarQuotation where

import Junior.TypeChecker.TypedAst (TypedExp, TypedExpF)
import Junior.Utils.RecursionSchemes (cataRec)
import Junior.Utils.Fixpoint (Fix(In))
import Junior.Utils.Annotations (Ann(Ann))
import Junior.Core.Ast (ExpF(Var, Lit, AppClosure, Let, VarPat))
import Junior.Parser.Primitives (Prim(S))

desugar :: [TypedExp] -> [TypedExp]
desugar es = (cataRec alg <$> es)
    where
        alg :: TypedExpF (TypedExp) -> TypedExp
        alg (Ann _ (Let (In (Ann _ (VarPat n1))) v (In (Ann _ (Var n2))))) | n1 == n2 = v 
        alg (Ann attr1 (AppClosure e1@(In (Ann _ (Var "quote"))) (In (Ann attr3 (Var f))))) = 
            In (Ann attr1 (AppClosure e1 (In (Ann attr3 (Lit (S f))))))
        alg x = In x


