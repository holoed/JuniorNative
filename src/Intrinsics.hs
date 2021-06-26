module Intrinsics where

import qualified Data.Set as Set
import Types ( Type(..), Qual((:=>)), Pred(IsIn) )
import Environment ( Env, toEnv )

tyLam :: Type -> Type -> Type
tyLam t1 = TyApp (TyApp (TyCon "->") t1)

env :: Env
env = toEnv [
  (".",  Set.fromList [] :=> tyLam (tyLam (TyVar "b" 0) (TyVar "c" 0)) 
                             (tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) 
                             (tyLam (TyVar "a" 0) (TyVar "c" 0)))),
  ("id", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("&&",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),
  ("||",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),                  
  ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
  ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
  ("null", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
  ("empty", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
  ("hd", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
  ("tl", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
  ("cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
  ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0)),
  ("toDouble", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "Double")),
  ("truncate", Set.fromList [] :=> tyLam (TyCon "Double") (TyCon "Int"))
 ]

classEnv :: [Qual Pred]
classEnv = [
  Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
  ]