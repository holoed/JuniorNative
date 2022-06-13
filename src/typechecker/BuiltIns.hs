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

charCon :: Type 
charCon = TyCon "Char"

tupleCon :: [Type] -> Type
tupleCon = foldl TyApp (TyCon "Tuple")
