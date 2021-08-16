module Types where

import Data.Set (Set, empty, union, singleton, unions, toList, filter, empty, (\\))
import Prelude hiding (null, map, filter)

-- Qualified 

data Pred = IsIn String Type deriving (Eq, Ord)

data Qual t = Set Pred :=> t deriving Eq

-- Type

data Type = TyCon String 
          | TyVar String Int 
          | TyApp Type Type deriving (Eq, Ord)

tyLam :: Type -> Type -> Type
tyLam t1 = TyApp (TyApp (TyCon "->") t1)

isLam :: Type -> Bool
isLam (TyApp (TyApp (TyCon "->") _) _) = True 
isLam _ = False

-- Type Schemes

data TypeScheme = ForAll (Set String) (Qual Type)
                | Identity (Qual Type) 

getTVarsOfType :: Type -> Set (String, Int)
getTVarsOfType (TyVar n k) = singleton (n, k)
getTVarsOfType (TyApp t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon _) = empty

getTVarsOfPred :: Pred -> Set (String, Int)
getTVarsOfPred (IsIn _ t) = getTVarsOfType t

getTVarsOfQType :: Qual Type -> Set (String, Int)
getTVarsOfQType (ps :=> t) = unions (fmap getTVarsOfPred (toList ps)) `union` getTVarsOfType t 

getTVarsOfQPred :: Qual Pred -> Set (String, Int)
getTVarsOfQPred (ps :=> p) = unions (fmap getTVarsOfPred (toList ps)) `union` getTVarsOfPred p 

clean :: Qual Type -> Qual Type 
clean (ps :=> t) = filter p ps :=> t
  where tvs = getTVarsOfType t 
        p x = getTVarsOfPred x \\ tvs == empty

deleteTautology :: Qual Type -> Qual Type
deleteTautology (ps :=> t) = filter p ps :=> t
  where p (IsIn _ t') = getTVarsOfType t' /= empty