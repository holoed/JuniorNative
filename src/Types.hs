module Types where

import Data.List (intercalate)
import Data.Set (Set, empty, union, singleton, null, foldl, map, unions, toList, filter, empty, (\\), size)
import Prelude hiding (null, map, filter)

-- Qualified 

data Pred = IsIn String Type deriving (Eq, Ord)

data Qual t = Set Pred :=> t deriving Eq

instance Show Pred where
  show (IsIn n t) = n ++ " " ++ show t 

instance Show a => Show (Qual a) where
  show (ps :=> t) = 
    if (null ps) then show t
    else
      let cs =  Data.Set.foldl (\acc x -> if acc /= "" then acc ++ ", " ++ (show x) else show x) "" ps in
      let cs' = if size ps > 1 then "(" ++ cs ++ ")" else cs in
      cs' ++ " => " ++ show t

-- Type

data Type = TyCon String 
          | TyVar String Int 
          | TyApp Type Type
          | TyLam Type Type deriving (Eq, Ord)

instance Show Type where
  show (TyCon name) = name
  show (TyVar name _) = name
  show (TyApp (TyApp (TyCon "Tuple") t1) t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TyApp (TyApp (TyApp (TyCon "Tuple") t1) t2) t3) = "(" ++ show t1 ++ ", " ++ show t2 ++ ", " ++ show t3 ++ ")"
  show (TyApp t1 t2) = show t1 ++ " " ++ show t2
  show (TyLam t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show _ = error "Not yet supported"

-- Type Schemes

data TypeScheme = ForAll (Set String) (Qual Type)
                | Identity (Qual Type)

getTVarsOfType :: Type -> Set String
getTVarsOfType (TyVar n _) = singleton n
getTVarsOfType (TyApp t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyLam t1 t2) = getTVarsOfType t1 `union` getTVarsOfType t2
getTVarsOfType (TyCon _) = empty

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