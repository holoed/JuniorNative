module Junior.Compiler.Intrinsics where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Junior.Core.Types ( Type(..), Qual((:=>)), Pred(IsIn), tyLam )
import Junior.TypeChecker.Environment ( Env, toEnv )
import Junior.TypeChecker.ContextReduction (ClassEnv(..))
import Junior.Core.BuiltIns ( doubleCon, intCon, tupleCon )

env :: Env
env = toEnv [
  ("==", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("/=", Set.fromList [IsIn "Eq" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("-",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("+",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("*",  Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("/",  Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("^",  Set.fromList [IsIn "Num" (TyVar "a" 0), IsIn "Integral" (TyVar "b" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "b" 0) (TyVar "a" 0))),
  (">",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("<",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  (">=",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("<=",  Set.fromList [IsIn "Ord" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyCon "Bool"))),
  ("&&",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),
  ("||",  Set.fromList [] :=> tyLam (TyCon "Bool") (tyLam (TyCon "Bool") (TyCon "Bool"))),
  ("null", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Bool")),
  ("nullStr", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "Bool")),
  ("[]", Set.fromList [] :=> TyApp (TyCon "List") (TyVar "a" 0)),
  ("()", Set.fromList [] :=> TyCon "Unit"),
  ("head", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyVar "a" 0)),
  ("tail", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0))),
  ("headStr", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "Char")),
  ("tailStr", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "String")),
  (":", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("!!", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (tyLam (TyCon "Int") (TyVar "a" 0))),
  ("fromInteger", Set.fromList [IsIn "Num" (TyVar "a" 0)] :=> tyLam (TyCon "Int") (TyVar "a" 0)),
  ("fromRational", Set.fromList [IsIn "Fractional" (TyVar "a" 0)] :=> tyLam (TyCon "Double") (TyVar "a" 0)),
  ("toDouble", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "Double")),
  ("truncate", Set.fromList [IsIn "Fractional" (TyVar "a" 0), IsIn "Integral" (TyVar "b" 0)] :=> tyLam (TyVar "a" 0) (TyVar "b" 0)),
  ("log", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("cos", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("sin", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("sqrt", Set.fromList [IsIn "Floating" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("fmap", Set.fromList [IsIn "Functor" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0))) ),
  ("pure", Set.fromList [IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (TyVar "a" 0) (TyApp (TyVar "f" 1) (TyVar "a" 0))),
  ("<*>", Set.fromList [IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (TyApp (TyVar "f" 1) (tyLam (TyVar "a" 0) (TyVar "b" 0))) (tyLam (TyApp (TyVar "f" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1) (TyVar "b" 0)))),
  (">=>", Set.fromList [IsIn "Monad" (TyVar "m" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "m" 1) (TyVar "b" 0))) (tyLam (tyLam (TyVar "b" 0) (TyApp (TyVar "m" 1) (TyVar "c" 0))) (tyLam (TyVar "a" 0) (TyApp (TyVar "m" 1) (TyVar "c" 0))))),
  (">>=", Set.fromList [IsIn "Monad" (TyVar "m" 1)] :=> tyLam (TyApp (TyVar "m" 1) (TyVar "a" 0)) (tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "m" 1) (TyVar "b" 0))) (TyApp (TyVar "m" 1) (TyVar "b" 0)))  ),
  ("runReader", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Reader") (TyVar "a" 0)) (TyVar "b" 0)) (tyLam (TyVar "a" 0) (TyVar "b" 0))),
  ("out", Set.fromList [] :=> tyLam (TyApp (TyCon "Fix") (TyVar "f" 1)) (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1)))),
  ("In", Set.fromList [] :=> tyLam (TyApp (TyVar "f" 1) (TyApp (TyCon "Fix") (TyVar "f" 1))) (TyApp (TyCon "Fix") (TyVar "f" 1))),
  ("mkParser", Set.fromList [] :=> tyLam (tyLam (TyCon "String") (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyCon "String")))) (TyApp (TyCon "Parser") (TyVar "a" 0))),
  ("runParser", Set.fromList [] :=> tyLam (TyApp (TyCon "Parser") (TyVar "a" 0)) (tyLam (TyCon "String") (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyCon "String"))))),
  ("toCharList", Set.fromList [] :=> tyLam (TyCon "String") (TyApp (TyCon "List") (TyCon "Char"))),
  ("fromCharList", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyCon "Char")) (TyCon "String")),
  ("charToStr", Set.fromList [] :=> tyLam (TyCon "Char") (TyCon "String")),
  ("ord", Set.fromList [] :=> tyLam (TyCon "Char") (TyCon "Int")),
  ("display", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyCon "List") (TyApp (TyApp (TyApp (TyCon "Tuple") (TyCon "Int")) (TyCon "Int")) (TyCon "Int")))) (TyApp (TyCon "Async") (TyCon "Unit"))),
  ("renderPlot", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyCon "List") (TyCon "Double"))) (TyApp (TyCon "Async") (TyCon "Unit"))),
  ("renderTimeSeries", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (tupleCon [TyCon "String", (TyApp (TyCon "List") (TyCon "String")), (TyApp (TyCon "List") (TyCon "Double"))])) (TyApp (TyCon "Async") (TyCon "Unit"))),
  ("renderDataGrid", Set.fromList [] :=> tyLam (TyCon "Json") (TyApp (TyCon "Async") (TyCon "Unit"))),
  ("range", Set.fromList[] :=> tyLam (tyLam (TyCon "Int") (TyVar "a" 0)) (tyLam (TyCon "Int") (tyLam (TyCon "Int") (TyApp (TyCon "List") (TyVar "a" 0))))),
  ("split", Set.fromList[] :=> tyLam (TyCon "Int") (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyApp (TyCon "List") (TyVar "a" 0))))),
  ("mod", Set.fromList [IsIn "Integral" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("fromListToMap", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyApp (TyApp (TyCon "Tuple") (TyVar "a" 0)) (TyVar "b" 0))) (TyApp (TyApp (TyCon "Map") (TyVar "a" 0)) (TyVar "b" 0))),
  ("undefined", Set.fromList [] :=> TyVar "a" 0),
  ("httpGet", Set.fromList [] :=> tyLam (TyCon "String") (TyApp (TyCon "Async") (TyCon "String"))),
  ("decompress", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "Uint8Array")),
  ("bufferToIntList", Set.fromList [] :=> tyLam (TyCon "Uint8Array") (TyApp (TyCon "List") (TyCon "Int"))),
  ("trace", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyVar "a" 0)),
  ("mkAsync", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "Async") (TyVar "a" 0))),
  ("JsonNode", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (tupleCon [TyCon "String", TyCon "Json"])) (TyCon "Json")),
  ("JsonValue", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "Json")),
  ("timeStampToDate", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "String")),
  ("isJust", Set.fromList [] :=> tyLam (TyApp (TyCon "Maybe") (TyVar "a" 0)) (TyCon "Bool")),
  ("fromJust", Set.fromList [] :=> tyLam (TyApp (TyCon "Maybe") (TyVar "a" 0)) (TyVar "a" 0)),
  ("Just", Set.fromList [] :=> tyLam (TyVar "a" 0) (TyApp (TyCon "Maybe") (TyVar "a" 0))),
  ("Nothing", Set.fromList [] :=> TyApp (TyCon "Maybe") (TyVar "a" 0)),
  ("parseJson", Set.fromList [] :=> tyLam (TyCon "String") (TyApp (TyCon "Maybe") (TyCon "Json"))),
  ("getJsonValue", Set.fromList [] :=> tyLam (TyCon "String") (tyLam (TyCon "Json") (TyApp (TyCon "Maybe") (TyCon "Json")))),
  ("getJsonList", Set.fromList [] :=> tyLam (TyCon "String") (tyLam (TyCon "Json") (TyApp (TyCon "Maybe") (TyApp (TyCon "List") (TyCon "Json"))))),
  ("jsonToInt", Set.fromList [] :=> tyLam (TyCon "Json") (TyApp (TyCon "Maybe") (TyCon "Int"))),
  ("jsonToDouble", Set.fromList [] :=> tyLam (TyCon "Json") (TyApp (TyCon "Maybe") (TyCon "Double"))),
  ("jsonToString", Set.fromList [] :=> tyLam (TyCon "Json") (TyApp (TyCon "Maybe") (TyCon "String"))),
  ("stringToJson", Set.fromList [] :=> tyLam (TyCon "String") (TyCon "Json")),
  ("intToJson", Set.fromList [] :=> tyLam (TyCon "Int") (TyCon "Json")),
  ("traverse", Set.fromList [IsIn "Traversable" (TyVar "t" 1), IsIn "Applicative" (TyVar "f" 1)] :=> tyLam (tyLam (TyVar "a" 0) (TyApp (TyVar "f" 1) (TyVar "b" 0))) (tyLam (TyApp (TyVar "t" 1) (TyVar "a" 0)) (TyApp (TyVar "f" 1)(TyApp (TyVar "t" 1) (TyVar "b" 0))))),
  ("foldr", Set.fromList [IsIn "Foldable" (TyVar "t" 1)] :=> tyLam (tyLam (TyVar "a" 0) (tyLam (TyVar "b" 0) (TyVar "b" 0))) (tyLam (TyVar "b" 0) (tyLam (TyApp (TyVar "t" 1) (TyVar "a" 0)) (TyVar "b" 0)))),
  ("foldl", Set.fromList [IsIn "Foldable" (TyVar "t" 1)] :=> tyLam (tyLam (TyVar "b" 0) (tyLam (TyVar "a" 0) (TyVar "b" 0))) (tyLam (TyVar "b" 0) (tyLam (TyApp (TyVar "t" 1) (TyVar "a" 0)) (TyVar "b" 0)))),
  ("<>", Set.fromList [IsIn "Semigroup" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (tyLam (TyVar "a" 0) (TyVar "a" 0))),
  ("fromMaybe", Set.fromList [] :=> tyLam (TyVar "a" 0) (tyLam (TyApp (TyCon "Maybe") (TyVar "a" 0)) (TyVar "a" 0))),
  ("fromMaybeLazy", Set.fromList [] :=> tyLam (tyLam (TyCon "Unit") (TyVar "a" 0)) (tyLam (TyApp (TyCon "Maybe") (TyVar "a" 0)) (TyVar "a" 0))),
  ("error", Set.fromList [] :=> tyLam (TyCon "String") (tyLam (TyCon "Unit") (TyVar "a" 0))),
  ("show", Set.fromList [IsIn "Show" (TyVar "a" 0)] :=> tyLam (TyVar "a" 0) (TyCon "String")),
  ("mapToJson", Set.fromList [] :=> tyLam (TyApp (TyApp (TyCon "Map") (TyCon "String")) (TyCon "Json")) (TyCon "Json")),
  ("listToJson", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyCon "Json")) (TyCon "Json")),
  ("jsonToList", Set.fromList [] :=> tyLam (TyCon "Json") (TyApp (TyCon "List") (TyCon "Json"))),
  ("length", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyCon "Int")),
  ("unlines", Set.fromList [] :=> tyLam (TyApp (TyCon "List") (TyCon "String")) (TyCon "String")),
  ("take", Set.fromList [] :=> tyLam (TyCon "Int") (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("drop", Set.fromList [] :=> tyLam (TyCon "Int") (tyLam (TyApp (TyCon "List") (TyVar "a" 0)) (TyApp (TyCon "List") (TyVar "a" 0)))),
  ("quote", Set.fromList [] :=> tyLam (tyLam (TyVar "a" 0) (TyVar "b" 0)) (TyApp (TyApp (TyCon "Quotation") (TyVar "a" 0)) (TyVar "b" 0))),
  ("remote", Set.fromList [] :=> tyLam ((TyApp (TyApp (TyCon "Quotation") (TyVar "a" 0)) (TyVar "b" 0))) (tyLam (TyVar "a" 0) (TyApp (TyCon "Async") (TyVar "b" 0))))
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
       Set.fromList [] :=> IsIn "Functor" (TyCon "Maybe"),
       Set.fromList [] :=> IsIn "Functor" (TyCon "Async"),
       Set.fromList [] :=> IsIn "Functor" (TyCon "List"),
       Set.fromList [] :=> IsIn "Functor" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Functor" (TyApp (TyCon "Reader") (TyVar "a" 0))
       ])),
     ("Applicative", (["Functor"], [
       Set.fromList [] :=> IsIn "Applicative" (TyCon "Maybe"),
       Set.fromList [] :=> IsIn "Applicative" (TyCon "Async"),
       Set.fromList [] :=> IsIn "Applicative" (TyCon "List"),
       Set.fromList [] :=> IsIn "Applicative" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Applicative" (TyApp (TyCon "Reader") (TyVar "a" 0))
       ])),
     ("Monad", (["Applicative"], [
       Set.fromList [] :=> IsIn "Monad" (TyCon "Maybe"),
       Set.fromList [] :=> IsIn "Monad" (TyCon "Async"),
       Set.fromList [] :=> IsIn "Monad" (TyCon "List"),
       Set.fromList [] :=> IsIn "Monad" (TyCon "Parser"),
       Set.fromList [] :=> IsIn "Monad" (TyApp (TyCon "Reader") (TyVar "a" 0))
      ])),
      ("Foldable", ([], [
       Set.fromList [] :=> IsIn "Foldable" (TyCon "Maybe"),
       Set.fromList [] :=> IsIn "Foldable" (TyCon "List")
      ])),
      ("Traversable", (["Functor", "Foldable"], [
       Set.fromList [] :=> IsIn "Traversable" (TyCon "Async"),
       Set.fromList [] :=> IsIn "Traversable" (TyCon "Maybe"),
       Set.fromList [] :=> IsIn "Traversable" (TyCon "List")
      ])),
      ("Semigroup", ([], [
        Set.fromList [] :=> IsIn "Semigroup" (TyCon "String"),
        Set.fromList [] :=> IsIn "Semigroup" (TyApp (TyCon "List") (TyVar "a" 0))
      ])),
      ("Show", ([], [
        Set.fromList [] :=> IsIn "Show" (TyCon "Int"),
        Set.fromList [IsIn "Show" (TyVar "a" 0)] :=> IsIn "Show" (TyApp (TyCon "List") (TyVar "a" 0))
      ])),
      ("Serializable", ([], [
        Set.fromList [] :=> IsIn "Serializable" (TyCon "Int")
      ]))
   ],
  defaults = [intCon, doubleCon]
}