module Substitutions where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
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
substitute s (TyLam t1 t2) = TyLam (substitute s t1) (substitute s t2)
substitute s (TyApp t1 t2) = TyApp (substitute s t1) (substitute s t2)
substitute s (TyCon name) = TyCon name

substitutePredicate :: Substitutions -> Pred -> Pred
substitutePredicate s (IsIn name t) = IsIn name (substitute s t) 

substitutePredicates :: Substitutions -> Set.Set Pred -> Set.Set Pred
substitutePredicates s = Set.map (substitutePredicate s)

substituteQ :: Substitutions -> Qual Type -> Qual Type
substituteQ s (ps :=> t) = substitutePredicates s ps :=> substitute s t