module DesugarRemotes where

import TypedAst (TypedExp, TypedExpF)
import RecursionSchemes (cataRec)
import Fixpoint (Fix(In))
import Annotations (Ann(Ann))
import Ast (ExpF(Var, App, Lit))
import Primitives (Prim(S))

desugar :: [TypedExp] -> [TypedExp]
desugar es = (cataRec alg <$> es)
    where
        alg :: TypedExpF (TypedExp) -> TypedExp
        alg (Ann attr0 (App (In (Ann attr1 (App (In (Ann attr2 (App e1@(In (Ann _ (Var "remote"))) e2))) e3))) (In (Ann attr3 (Var f))))) = 
            In (Ann attr0 (App (In (Ann attr1 (App (In (Ann attr2 (App e1 e2))) e3))) (In (Ann attr3 (Lit (S f))))))
        alg x = In x



