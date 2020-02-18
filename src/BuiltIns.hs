module BuiltIns where

import Types

boolCon :: Type
boolCon = TyCon "Bool" 

intCon :: Type
intCon = TyCon "Int" 

strCon :: Type
strCon = TyCon "String" 

tupleCon :: [Type] -> Type
tupleCon = foldl TyApp (TyCon "Tuple")
