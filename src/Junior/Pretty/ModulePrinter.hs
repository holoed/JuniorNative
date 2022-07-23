module Junior.Pretty.ModulePrinter where

import Junior.Core.Ast (ExpF(..))
import Junior.Utils.Fixpoint (Fix(..))
import Junior.Utils.Annotations ( Ann(Ann), mapAnn )
import Junior.TypeChecker.TypedAst (TypedExp)
import Junior.Pretty.PrettyTypes (prettifyTypes)
import Junior.Pretty.TypesPrinter ()
import Junior.Pretty.Printer (prettyDoc, isop)
import Junior.Parser.SynExpToExp (fromExp)
import Text.PrettyPrint.Mainland ( (<+>), text, Doc, pretty, (</>), stack, parens )
import Data.Text ( Text, pack ) 

typedBindingToString :: TypedExp -> Doc
typedBindingToString e@(In (Ann (_, qt) (Defn _ (In (Ann _ (VarPat n))) _))) =
    let fnName x = if isop x then parens (text x) else text x in
    stack [text "val" <+> fnName n <+> text "::" <+> text (show qt),
          (prettyDoc . fromExp . mapAnn fst) e]
typedBindingToString e = (prettyDoc . fromExp . mapAnn fst) e

typedModuleToString :: [TypedExp] -> Text
typedModuleToString es =
    pack $ pretty 40 $ stack (flip (</>) (text ""). typedBindingToString . prettifyTypes <$> es)
