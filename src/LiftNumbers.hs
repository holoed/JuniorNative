module LiftNumbers where

import Primitives ( Prim(I) )
import Ast ( Exp, ExpF(Lit), lit, var, app, ExpLoc(..) )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Annotations (Ann(..))

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Ann (LitLoc l) (Lit (I n))) = app (var l "fromInteger") (lit l (I n))
       f x = In x