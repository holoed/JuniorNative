module Substitutions where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
import Prelude hiding (lookup)

type Substitutions = Map.Map String Type

extend :: String -> Type -> Substitutions -> Substitutions
extend = Map.insert

lookup :: String -> Substitutions -> Type
lookup v = fromMaybe (TyVar v) . Map.lookup v

substitute :: Substitutions -> Type -> Type
substitute s t@(TyVar n) = let t' = lookup n s in
                     if t == t' then t'
                     else substitute s t'
substitute s (TyLam a r) = TyLam (substitute s a) (substitute s r)
substitute s (TyCon name tyArgs) = TyCon name (fmap (s `substitute`) tyArgs)

substitutePredicate :: Substitutions -> Pred -> Pred
substitutePredicate s (IsIn name t) = IsIn name (substitute s t) 

substitutePredicates :: Substitutions -> Set.Set Pred -> Set.Set Pred
substitutePredicates s = Set.map (substitutePredicate s)

substituteQ :: Substitutions -> Qual Type -> Qual Type
substituteQ s (ps :=> t) = substitutePredicates s ps :=> substitute s t