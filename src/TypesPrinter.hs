module TypesPrinter where

import Types ( Type(..), Qual(..), Pred(..) )
import Text.PrettyPrint ( (<+>), (<>), render, text, Doc )
import Data.Set (size, foldl)
import Prelude hiding (Left, Right, (<>))
import Control.Monad.Writer ( runWriter, MonadWriter(tell, listen), Writer )
import Operators ( Operator, Associativity(Right, Left), lamOp )
import PrettyPrinter ( bracket )

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
toDoc (TyApp (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) t4) =
    do x1 <- toDoc t1
       x2 <- toDoc t2
       x3 <- toDoc t3
       x4 <- toDoc t4
       return $ text "(" <> x1 <> text "," <+> x2 <> text "," <+> x3 <> text "," <+> x4 <> text ")"
toDoc (TyApp (TyApp (TyCon "->") t1) t2) =
    do (x1, l) <- listen $ toDoc t1
       (x2, r) <- listen $ toDoc t2
       tell [lamOp]
       return (bracket Left lamOp l x1 <+> text "->" <+> bracket Right lamOp r x2)
toDoc (TyApp t1 t2) =
    do x1 <- toDoc t1
       x2 <- toDoc t2
       return $ x1 <+> x2

instance Show Type where
  show = render . fst . runWriter . toDoc

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t

instance Show a => Show (Qual a) where
  show (ps :=> t) =
    if null ps then show t
    else
      let cs =  Data.Set.foldl (\acc x -> if acc /= "" then acc ++ ", " ++ show x else show x) "" ps in
      let cs' = if size ps > 1 then "(" ++ cs ++ ")" else cs in
      cs' ++ " => " ++ show t
