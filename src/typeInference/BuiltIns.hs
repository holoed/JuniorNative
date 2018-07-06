module TypeInference.BuiltIns where

import TypeInference.Types

boolCon :: Type
boolCon = TyCon "Bool" []

intCon :: Type
intCon = TyCon "Int" []

tupleCon :: [Type] -> Type
tupleCon = TyCon "Tuple"
