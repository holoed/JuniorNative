module ModulePrinter where

import Ast (ExpF(..))
import Fixpoint (Fix(..))
import Annotations ( Ann(Ann), mapAnn )
import TypedAst (TypedExp)
import PrettyTypes (prettifyTypes)
import TypesPrinter ()
import PrettyPrinter (prettyDoc, isop)
import SynExpToExp (fromExp)
import Text.PrettyPrint.Mainland ( (<+>), text, Doc, pretty, (</>), stack, parens )

typedBindingToString :: TypedExp -> Doc
typedBindingToString e@(In (Ann (_, qt) (Let (In (Ann _ (VarPat n))) _ _))) =
    let fnName x = if isop x then parens (text x) else text x in
    stack [text "val" <+> fnName n <+> text "::" <+> text (show qt),
          (prettyDoc . fromExp . mapAnn fst) e]
typedBindingToString e = (prettyDoc . fromExp . mapAnn fst) e

typedModuleToString :: [TypedExp] -> String
typedModuleToString es =
    pretty 50 $ stack (flip (</>) (text ""). typedBindingToString . prettifyTypes <$> es)
