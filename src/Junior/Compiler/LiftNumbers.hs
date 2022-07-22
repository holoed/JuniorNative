module Junior.Compiler.LiftNumbers where

import Junior.Parser.Primitives ( Prim(I, D) )
import Junior.Core.Ast ( Exp, ExpF(Lit, Var, App), lit, var, app )
import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Utils.RecursionSchemes ( cataRec )
import Junior.Utils.Annotations (Ann(..))

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