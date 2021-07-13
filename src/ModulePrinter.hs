module ModulePrinter where

import Ast (ExpF(..))
import Fixpoint (Fix(..))
import Annotations ( Ann(Ann), mapAnn )
import Data.List (intercalate)
import TypedAst (TypedExp)
import PrettyTypes (prettifyTypes)
import TypesPrinter ()
import PrettyPrinter (pretty)
import SynExpToExp (fromExp)

typedBindingToString :: TypedExp -> String
typedBindingToString e@(In (Ann (_, qt) (Let (In (Ann _ (VarPat n))) _ _))) =
    intercalate "\n"
    ["val " ++ n ++ " :: " ++ show qt,
    (pretty . fromExp . mapAnn fst) e]
typedBindingToString e = (pretty . fromExp . mapAnn fst) e

typedModuleToString :: [TypedExp] -> String
typedModuleToString es =
    intercalate "\n\n" (typedBindingToString . prettifyTypes <$> es)
