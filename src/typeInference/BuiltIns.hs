module BuiltIns where

import Types

boolCon :: Type
boolCon = TyCon "Bool" []

intCon :: Type
intCon = TyCon "Int" []

tupleCon :: [Type] -> Type
tupleCon = TyCon "Tuple" 
