module Junior.Pretty.TypesPrinter where

import Junior.Core.Types ( Type(..), Qual(..), Pred(..) )
import Text.PrettyPrint.Mainland ( (<+>), text, Doc, pretty )
import Data.Semigroup ( Semigroup((<>)) )
import qualified Data.Set as Set
import Prelude hiding (Left, Right, (<>))
import Control.Monad.Writer ( runWriter, MonadWriter(tell, listen), Writer )
import Junior.Core.Operators ( Operator, Associativity(Right, Left), lamOp, juxtaOp )
import Junior.Pretty.Utils ( bracket )
import Control.Monad ( foldM )

foldDoc :: [Type] -> Writer [Operator] Doc
foldDoc xs = do
           x <- foldM (\acc x -> do x' <- toDoc x
                                    return ((acc x' <> text ",") <+>)) (text "(" <>) (init xs)
           last' <- toDoc $ last xs
           return $ x $ last' <> text ")"

takeLast :: [a] -> [a]
takeLast = take 1 . reverse

toDoc :: Type -> Writer [Operator] Doc
toDoc (TyCon name) = return $ text name
toDoc (TyVar name _) = return $ text name
toDoc (TyApp (TyApp (TyCon "Tuple") t1) t2) = foldDoc [t1, t2]
toDoc (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) = foldDoc [t1, t2, t3]
toDoc (TyApp (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) t4) = foldDoc [t1, t2, t3, t4]
toDoc (TyApp (TyApp (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) t4) t5) = foldDoc [t1, t2, t3, t4, t5]
toDoc (TyApp (TyApp (TyCon "->") t1) t2) =
    do (x1, l) <- listen $ toDoc t1
       (x2, r) <- listen $ toDoc t2
       tell [lamOp]
       return (bracket Left lamOp (takeLast l) x1 <+> text "->" <+> bracket Right lamOp (takeLast r) x2)
toDoc (TyApp t1 t2) =
    do (x1, l) <- listen $ toDoc t1
       (x2, r) <- listen $ toDoc t2
       tell [juxtaOp]
       return (bracket Left juxtaOp (takeLast l) x1 <+> bracket Right juxtaOp (takeLast r) x2)

instance Show Type where
  show = pretty 80 . fst . runWriter . toDoc

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t

instance Show a => Show (Qual a) where
  show (ps :=> t) =
    if null ps then show t
    else
      let cs =  Set.foldl (\acc x -> if acc /= "" then acc ++ ", " ++ show x else show x) "" ps in
      let cs' = if Set.size ps > 1 then "(" ++ cs ++ ")" else cs in
      cs' ++ " => " ++ show t
