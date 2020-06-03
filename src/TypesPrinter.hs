module TypesPrinter where

import Types
import Text.PrettyPrint
import Data.Set (size, foldl)
import Prelude hiding ((<>))

toDoc :: Type -> Doc
toDoc (TyCon name) = text name
toDoc (TyVar name _) = text name
toDoc (TyApp (TyApp (TyCon "Tuple") t1) t2) = 
    text "(" <> toDoc t1 <> text "," <+> toDoc t2 <> text ")"
toDoc (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) = 
    text "(" <> toDoc t1 <> text "," <+> toDoc t2 <> text "," <+> toDoc t3 <> text ")"
toDoc (TyApp t1 t2) = toDoc t1 <+> toDoc t2
toDoc (TyLam t1 t2) = text "(" <> toDoc t1 <+> text "->" <+> toDoc t2 <> text ")"

instance Show Type where
  show = render . toDoc 

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t 

instance Show a => Show (Qual a) where
  show (ps :=> t) = 
    if (null ps) then show t
    else
      let cs =  Data.Set.foldl (\acc x -> if acc /= "" then acc ++ ", " ++ (show x) else show x) "" ps in
      let cs' = if size ps > 1 then "(" ++ cs ++ ")" else cs in
      cs' ++ " => " ++ show t
