module LiftNumbers where

import Primitives ( Prim(I, D) )
import Ast ( Exp, ExpF(Lit), lit, var, app, ExpLoc(..) )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Annotations (Ann(..))

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Ann (LitLoc l) (Lit (I n))) = app (var l "fromInteger") (lit l (I n))
       f (Ann (LitLoc l) (Lit (D x))) = app (var l "fromRational") (lit l (D x))
       f x = In x