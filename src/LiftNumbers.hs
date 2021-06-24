module LiftNumbers where

import Primitives ( Prim(I, D) )
import Ast ( Exp, ExpF(Lit), lit, var, app )
import Fixpoint ( Fix(In) )
import RecursionSchemes ( cataRec )
import Annotations (Ann(..))

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Ann (Just l) (Lit (I n))) = app (var l "fromInteger") (lit l (I n))
       f (Ann (Just l) (Lit (D x))) = app (var l "fromRational") (lit l (D x))
       f x = In x