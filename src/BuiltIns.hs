module BuiltIns where

import Types ( Type(TyCon, TyApp) )

boolCon :: Type
boolCon = TyCon "Bool" 

intCon :: Type
intCon = TyCon "Int" 

doubleCon :: Type 
doubleCon = TyCon "Double"

strCon :: Type
strCon = TyCon "String" 

tupleCon :: [Type] -> Type
tupleCon = foldl TyApp (TyCon "Tuple")

untuple :: Type -> [Type]
untuple  = reverse . untuple'
 where untuple' (TyApp t1 t2) = t2 : untuple' t1
       untuple' (TyCon "Tuple") = []
       untuple' _ = undefined 