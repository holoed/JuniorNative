module LiftNumbers where

import Primitives ( Prim(I) )
import Ast ( Exp, ExpF(Lit), lit, var, app )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Lit (I n)) = app (var "fromInteger") (lit (I n))
       f x = In x