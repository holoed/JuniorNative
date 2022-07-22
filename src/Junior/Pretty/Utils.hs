module Junior.Pretty.Utils where

import Prelude hiding (Left, Right, (<>), pi)
import Junior.Core.Operators ( Operator, Fixity(Infix, Postfix, Prefix), Associativity(..) )
import Text.PrettyPrint.Mainland.Class ()
import Text.PrettyPrint.Mainland ( parens, Doc, parens )

noparens ::Operator -> Operator -> Associativity -> Bool
noparens (_, pi, fi) (_, po, fo) side = pi > po || other fi side
  where other Postfix Left  = True
        other Prefix  Right = True
        other (Infix Left) Left = pi == po && fo == Infix Left
        other (Infix Right) Right = pi == po && fo == Infix Right
        other _ NonAssoc = fi == fo
        other _ _ = False

bracket :: Associativity -> Operator -> [Operator] -> Doc -> Doc
bracket _ _ [] doc = doc
bracket side outer (inner:_) doc =
  if noparens inner outer side then doc else parens doc