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
  (">=",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("<=",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("&&",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),
  ("||",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),
  ("fst", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "a" 0)),
  ("snd", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0)) (TyVar "b" 0)),
  ("null", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
  ("[]", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
  ("()", Set.fromList [] :=> TyCon "Unit"),
  ("head", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
  ("tail", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
  (":", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
  ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0)),
  ("toDouble", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "Double")),
  ("truncate", Set.fromList [IsIn "Fractional" (TyVar "a" 0), IsIn "Integral" (TyVar "b" 0)] :=> tyLam (TyVar "a" 0) (TyVar "b" 0)),
  ("cos", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("sin", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("sqrt", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("fmap", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0))) ),
  ("pure", Set.fromList [IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (TyVar "a" 0) (TyApp (TyVar "f" 1) (TyVar "a" 0))),
  ("<*>", Set.fromList [IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (TyApp (TyVar "f" 1) (tyLam (TyVar "a" 0) (TyVar "b" 0))) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0)))),
  ("bind", Set.fromList [IsIn "Monad" (TyVar "m" 1)] :=> tyLam (TyApp (TyVar "m" 1) (TyVar "a" 0)) (tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "m" 1) (TyVar "b" 0))) (TyApp (TyVar "m" 1) (TyVar "b" 0)))  ),
  ("runReader", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Reader") (TyVar "a" 0)) (TyVar "b" 0)) (tyLam (TyVar "a" 0) (TyVar "b" 0))),
  ("out", Set.fromList [] :=> tyLam (TyApp (TyCon "Fix") (TyVar "f" 1)) (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1)))),
  ("In", Set.fromList [] :=> tyLam (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1))) (TyApp (TyCon "Fix") (TyVar "f" 1))),
  ("mkParser", Set.fromList [] :=> tyLam (tyLam (TyApp (TyCon "List") (TyCon "Char")) (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyApp (TyCon "List") (TyCon "Char"))))) (TyApp (TyCon "Parser") (TyVar "a" 0))),
  ("runParser", Set.fromList [] :=> tyLam (TyApp (TyCon "Parser") (TyVar "a" 0)) (tyLam (TyApp (TyCon "List") (TyCon "Char")) (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyApp (TyCon "List") (TyCon "Char")))))),
  ("toCharList", Set.fromList [] :=> tyLam (TyCon "String") (TyApp (TyCon "List") (TyCon "Char"))),
  ("ord", Set.fromList [] :=> tyLam (TyCon "Char") (TyCon "Int")),
  ("display", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyCon "List") (TyApp (TyApp (TyApp (TyCon "Tuple") (TyCon "Int")) (TyCon "Int")) (TyCon "Int")))) (TyApp (TyApp (TyCon "Map") (TyCon "String")) (TyApp (TyCon "List") (TyApp (TyCon "List") (TyApp (TyApp (TyApp (TyCon "Tuple") (TyCon "Int")) (TyCon "Int")) (TyCon "Int")))))),
  ("renderPlot", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyCon "List") (TyCon "Double"))) (TyApp (TyCon "List") (TyApp (TyCon "List") (TyCon "Double")))),
  ("range", Set.fromList[] :=> tyLam (tyLam (TyCon "Int") (TyVar "a" 0)) (tyLam (TyCon "Int") (tyLam (TyCon "Int") (TyApp (TyCon "List") (TyVar "a" 0))))),
  ("split", Set.fromList[] :=> tyLam (TyCon "Int") (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyApp (TyCon "List") (TyVar "a" 0))))),
  ("mod", Set.fromList [IsIn "Integral" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("fromListToMap", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))) (TyApp (TyApp (TyCon "Map") (TyVar "a" 0)) (TyVar "b" 0))),
  ("Empty", Set.fromList [] :=> TyApp (TyApp (TyCon "ListF") (TyVar "a" 0)) (TyVar "b" 0)),
  ("Cons", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "b" 0) (TyApp (TyApp (TyCon "ListF") (TyVar "a" 0)) (TyVar "b" 0)))),
  ("undefined", Set.fromList [] :=> TyVar "a" 0),
  ("isEmpty", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "ListF") (TyVar "a" 0)) (TyVar "b" 0)) (TyCon "Bool")),
  ("isCons", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "ListF") (TyVar "a" 0)) (TyVar "b" 0)) (TyCon "Bool")),
  ("extractCons", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "ListF") (TyVar "a" 0)) (TyVar "b" 0)) (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))),
  ("httpGet", Set.fromList [] :=> tyLam (TyCon "String") (TyApp (TyCon "Async") (TyCon "String"))),
  ("trace", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0))
 ]

classEnv :: ClassEnv
classEnv = ClassEnv {
  classes = Map.fromList [
     ("Eq", ([], [
       Set.fromList [] :=> IsIn "Eq" (TyCon "Bool"),
       Set.fromList [] :=> IsIn "Eq" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Eq" (TyCon "Double"),
       Set.fromList [] :=> IsIn "Eq" (TyCon "String"),
       Set.fromList [] :=> IsIn "Eq" (TyCon "Char"),
       Set.fromList [IsIn "Eq" (TyVar "a" 0), IsIn "Eq" (TyVar "b" 0)] :=> IsIn "Eq" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
      ])),
     ("Ord", (["Eq"], [
       Set.fromList [] :=> IsIn "Ord" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Ord" (TyCon "Double"),
       Set.fromList [] :=> IsIn "Ord" (TyCon "Char")
       ])),
     ("Num", ([], [
       Set.fromList [] :=> IsIn "Num" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Num" (TyCon "Double"),
       Set.fromList [IsIn "Num" (TyVar "a" 0), IsIn "Num" (TyVar "b" 0)] :=> IsIn "Num" (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))
       ])),
     ("Fractional", (["Num"], [
       Set.fromList [] :=> IsIn "Fractional" (TyCon "Int"),
       Set.fromList [] :=> IsIn "Fractional" (TyCon "Double")
       ])),
     ("Floating", (["Fractional"], [
       Set.fromList [] :=> IsIn "Floating" (TyCon "Double")
       ])),
      ("Integral", (["Num"], [
        Set.fromList [] :=> IsIn "Integral" (TyCon "Int")
      ])),
     ("Functor", ([], [
       Set.fromList [] :=> IsIn "Functor" (TyCon "Async"),
       Set.fromList [] :=> IsIn "Functor" (TyCon "List"),
       Set.fromList [] :=> IsIn "Functor" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Functor" (TyApp (TyCon "Reader") (TyVar "a" 0)),
       Set.fromList [] :=> IsIn "Functor" (TyApp (TyCon "ListF") (TyVar "a" 0))
       ])),
     ("Applicative", (["Functor"], [
       Set.fromList [] :=> IsIn "Applicative" (TyCon "List"),
       Set.fromList [] :=> IsIn "Applicative" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Applicative" (TyApp (TyCon "Reader") (TyVar "a" 0))
       ])),
     ("Monad", (["Applicative"], [
       Set.fromList [] :=> IsIn "Monad" (TyCon "List"),
       Set.fromList [] :=> IsIn "Monad" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Monad" (TyApp (TyCon "Reader") (TyVar "a" 0))
      ]))
   ],
  defaults = [intCon, doubleCon]
}