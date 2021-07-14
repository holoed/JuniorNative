module LiftNumbers where

import Primitives ( Prim(I, D) )
import Ast ( Exp, ExpF(Lit, Var, App), lit, var, app )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Annotations (Ann(..))

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Ann (Just l) (Lit (I n))) = app (var l "fromInteger") (lit l (I n))
       f (Ann (Just l) (Lit (D x))) = app (var l "fromRational") (lit l (D x))
       f x = In x

dropN :: Exp -> Exp
dropN = cataRec f
 where f (Ann _ (App (In (Ann _ (Var "fromInteger"))) e)) = e
       f (Ann _ (App (In (Ann _ (Var "fromRational"))) e)) = e
       f x = In x