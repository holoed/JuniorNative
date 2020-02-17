module LiftNumbers where

import Ast
import Fixpoint
import RecursionSchemes

liftN :: Exp -> Exp
liftN = cataRec f
 where f (Lit (I n)) = app (var "fromInteger") (lit (I n))
       f x = In x