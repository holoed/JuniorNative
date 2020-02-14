module Types where

import Data.List (intercalate)
import Data.Set (Set, empty, union, singleton, null, foldl, map, unions, toList, filter, empty, (\\))
import Prelude hiding (null, map, filter)

-- Qualified 

data Pred = IsIn String Type deriving (Eq, Ord)

data Qual t = Set Pred :=> t deriving Eq

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t 

instance Show a => Show (Qual a) where
  show (ps :=> t) = 
    if (null ps) then show t
    else (Data.Set.foldl (\acc x -> if acc /= "" then acc ++ "," ++ (show x) else show x) "" ps) ++ " => " ++ show t

-- Type

data Type = TyCon String [Type]
          | TyVar String
          | TyLam Type Type deriving (Eq, Ord)

instance Show Type where
  show (TyCon name []) = name
  show (TyCon "Tuple" xs) = "(" ++ intercalate ", " (fmap show xs) ++ ")"
  show (TyVar name) = name
  show (TyLam t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show _ = error "Not yet supported"

-- Type Schemes

data TypeScheme = ForAll (Set String) (Qual Type)
                | Identity (Qual Type)

getTVarsOfType :: Type -> Set String
getTVarsOfType (TyVar n) = singleton n
getTVarsOfType (TyLam t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon _ args) = Prelude.foldl (\ acc t -> acc `union` getTVarsOfType t) empty args

getTVarsOfPred :: Pred -> Set String
getTVarsOfPred (IsIn _ t) = getTVarsOfType t

getTVarsOfQType :: Qual Type -> Set String
getTVarsOfQType (ps :=> t) = unions (fmap getTVarsOfPred (toList ps)) `union` getTVarsOfType t 

clean :: Qual Type -> Qual Type 
clean (ps :=> t) = filter p ps :=> t
  where tvs = getTVarsOfType t 
        p x = getTVarsOfPred x \\ tvs == empty

deleteTautology :: Qual Type -> Qual Type
deleteTautology (ps :=> t) = filter p ps :=> t
  where p (IsIn _ t') = getTVarsOfType t' /= empty