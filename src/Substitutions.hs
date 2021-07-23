module Substitutions where

import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types ( Type(..), Qual(..), Pred(..) )
import Prelude hiding (lookup)

type Substitutions = Map.Map (String, Int) Type

extend :: (String, Int) -> Type -> Substitutions -> Substitutions
extend = Map.insert

lookup :: (String, Int) -> Substitutions -> Type
lookup (n, k) = fromMaybe (TyVar n k) . Map.lookup (n, k)

substitute :: Substitutions -> Type -> Type
substitute s t@(TyVar n k) = let t' = lookup (n, k) s in
                     if t == t' then t'
                     else substitute s t'
substitute s (TyApp t1 t2) = TyApp (substitute s t1) (substitute s t2)
substitute _ (TyCon name) = TyCon name

substitutePredicate :: Substitutions -> Pred -> Pred
substitutePredicate s (IsIn name t) = IsIn name (substitute s t) 

substitutePredicates :: Substitutions -> Set.Set Pred -> Set.Set Pred
substitutePredicates s = Set.map (substitutePredicate s)

substituteQ :: Substitutions -> Qual Type -> Qual Type
substituteQ s (ps :=> t) = substitutePredicates s ps :=> substitute s t

mappings :: Type -> Type -> Either (Type, Type) Substitutions
mappings (TyApp t1 t2) (TyApp t1' t2') = do x <- mappings t1 t1'
                                            y <- mappings t2 t2'
                                            return $ x `Map.union` y
mappings (TyCon _) (TyCon _) = return Map.empty 
mappings (TyVar n k) t2 = return $ Map.fromList [((n, k), t2)]
mappings t1 (TyVar n k) = return $ Map.fromList [((n, k), t1)]
mappings t1 t2 = Left (t1, t2)