module TypesPrinter where

import Types
import Text.PrettyPrint
import Data.Set (size, foldl)
import Prelude hiding ((<>))
import Control.Monad.Writer hiding ((<>))
import Operators

toDoc :: Type -> Writer [Operator] Doc
toDoc (TyCon name) = return $ text name
toDoc (TyVar name _) = return $ text name
toDoc (TyApp (TyApp (TyCon "Tuple") t1) t2) = 
    do x1 <- toDoc t1
       x2 <- toDoc t2
       return $ text "(" <> x1 <> text "," <+> x2 <> text ")"
toDoc (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) = 
    do x1 <- toDoc t1
       x2 <- toDoc t2
       x3 <- toDoc t3
       return $ text "(" <> x1 <> text "," <+> x2 <> text "," <+> x3 <> text ")"
toDoc (TyApp t1 t2) = 
    do x1 <- toDoc t1
       x2 <- toDoc t2
       return $ x1 <+> x2
toDoc (TyLam t1 t2) = 
    do x1 <- toDoc t1
       x2 <- toDoc t2
       return $ text "(" <> x1 <+> text "->" <+> x2 <> text ")"

instance Show Type where
  show = render . fst . runWriter . toDoc 

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t 

instance Show a => Show (Qual a) where
  show (ps :=> t) = 
    if (null ps) then show t
    else
      let cs =  Data.Set.foldl (\acc x -> if acc /= "" then acc ++ ", " ++ (show x) else show x) "" ps in
      let cs' = if size ps > 1 then "(" ++ cs ++ ")" else cs in
      cs' ++ " => " ++ show t
