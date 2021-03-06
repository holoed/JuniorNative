module Intrinsics where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Types ( Type(..), Qual((:=>)), Pred(IsIn), tyLam )
import Environment ( Env, toEnv )
import ContextReduction (ClassEnv(..))
import BuiltIns ( doubleCon, intCon )

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
  ("[]", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
  ("head", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
  ("tail", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
  (":", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
  ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0)),
  ("toDouble", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "Double")),
  ("truncate", Set.fromList [] :=> tyLam (TyCon "Double") (TyCon "Int")),
  ("cos", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("sin", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("fmap", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0))) ),
  ("pure", Set.fromList [IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (TyVar "a" 0) (TyApp (TyVar "f" 1) (TyVar "a" 0))),
  ("bind", Set.fromList [IsIn "Monad" (TyVar "m" 1)] :=> tyLam (TyApp (TyVar "m" 1) (TyVar "a" 0)) (tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "m" 1) (TyVar "b" 0))) (TyApp (TyVar "m" 1) (TyVar "b" 0)))  ),
  ("runReader", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Reader") (TyVar "a" 0)) (TyVar "b" 0)) (tyLam (TyVar "a" 0) (TyVar "b" 0))),
  ("fixOut", Set.fromList [] :=> tyLam (TyApp (TyCon "Fix") (TyVar "f" 1)) (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1)))),
  ("fixIn", Set.fromList [] :=> tyLam (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1))) (TyApp (TyCon "Fix") (TyVar "f" 1)))
 ]

classEnv :: ClassEnv
classEnv = ClassEnv { 
  classes = Map.fromList [
     ("Eq", ([], [
       Set.fromList [] :=> IsIn "Eq" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Eq" (TyCon "Double"),
       Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
      ])),
     ("Ord", (["Eq"], [Set.fromList [] :=> IsIn "Ord" (TyCon "Double")])),
     ("Num", ([], [
       Set.fromList [] :=> IsIn "Num" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Num" (TyCon "Double")
       ])),
     ("Fractional", (["Num"], [Set.fromList [] :=> IsIn "Fractional" (TyCon "Double")])),
     ("Floating", (["Fractional"], [Set.fromList [] :=> IsIn "Floating" (TyCon "Double")])),

     ("Functor", ([], [
       Set.fromList [] :=> IsIn "Functor" (TyCon "List"),
       Set.fromList [] :=> IsIn "Functor" (TyApp (TyCon "Reader") (TyVar "a" 0))
       ])),
     ("Applicative", (["Functor"], [
       Set.fromList [] :=> IsIn "Applicative" (TyCon "List"),
       Set.fromList [] :=> IsIn "Applicative" (TyApp (TyCon "Reader") (TyVar "a" 0))
       ])),
     ("Monad", (["Applicative"], [
       Set.fromList [] :=> IsIn "Monad" (TyCon "List"),
       Set.fromList [] :=> IsIn "Monad" (TyApp (TyCon "Reader") (TyVar "a" 0))
      ]))
   ],
  defaults = [intCon, doubleCon]
}