{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module JavascriptGenerator where

import Ast ( ExpF(Lit, Var, VarPat, Lam, App, Let, Defn, IfThenElse, MkTuple, TuplePat, MkClosure, AppClosure, GetEnv, SetEnv), TypeDecl (TypeDecl) )
import TypedAst ( TypedExp )
import Data.Text (Text, intercalate, pack, isPrefixOf, replace)
import RecursionSchemes ( cataRec )
import Primitives ( Prim(..) )
import Annotations (unwrap)
import Fixpoint ( Fix(In) )
import Prelude hiding (dropWhile)
import Data.String.Interpolate ( i )
import Types (Type (TyCon, TyApp))
 
generatePrim :: Prim -> Text
generatePrim (I n) = (pack . show) n
generatePrim (D x) = (pack . show) x
generatePrim (B b) = if b then "true" else "false"
generatePrim (S s) = "\"" <> pack s <> "\""
generatePrim (C c) = "\'" <> pack [c] <> "\'"
generatePrim U = "()"

mapOp :: Text -> Text
mapOp "+" = "__add"
mapOp "*" = "__mul"
mapOp "==" = "__eqeq"
mapOp "/=" = "__noteq"
mapOp "||" = "__or"
mapOp "&&" = "__and"
mapOp "-"  = "__sub"
mapOp "/"  = "__div"
mapOp ">"  = "__gt"
mapOp "<"  = "__lt"
mapOp ">="  = "__gteq"
mapOp "<="  = "__lteq"
mapOp ":"  = "__colon"
mapOp "."  = "__dot"
mapOp "<*>" = "__liftA2"
mapOp ">=>" = "__lrKleisli"
mapOp ">>=" = "__bind"
mapOp "<>" = "mappend"
mapOp "++" = "__plusplus"
mapOp x    = x

mapName :: Text -> Text
mapName "null" = "isEmptyList"
mapName "eval" = "__eval"
mapName "mod"  = "__mod"
mapName "const" = "__const"
mapName "()" = "undefined"
mapName "." = "__dot"
mapName ":" = "__colon"
mapName "++" = "__plusplus"
mapName ">=>" = "__lrKleisli"
mapName x      = replace "'" "Quoted" x

generateExp :: Fix ExpF -> Text
generateExp = cataRec alg
    where alg (Lit p) = generatePrim p
          alg (Var n) = mapName (pack n)
          alg (VarPat n) = mapName (pack n)
          alg (MkTuple xs) = pack "[" <> intercalate (pack ",") xs <> pack "]"
          alg (TuplePat xs) = pack "[" <> intercalate (pack ",") xs <> pack "]"
          alg (Lam s e) = "function (" <> s <> ") { " <> (if isPrefixOf "if" e || isPrefixOf "const" e then "" else " return ") <> e <> " }"
          alg (App "nativeAddInt" e) =
              e <> "+"
          alg (App "nativeSubInt" e) =
              e <> "-"
          alg (App "nativeMulInt" e) =
              e <> "*"
          alg (App "nativeDivInt" e) =
              "div" <> "(" <> e <> ","
          alg (App "nativeEqInt" e) =
              e <> "=="
          alg (App "nativeAddDouble" e) =
              e <> "+"
          alg (App "nativeSubDouble" e) =
              e <> "-"
          alg (App "nativeMulDouble" e) =
              e <> "*"
          alg (App "nativeDivDouble" e) =
              e <> "/"
          alg (App "nativeEqDouble" e) =
              e <> "=="
          alg (App "nativeGtDouble" e) =
              e <> ">"
          alg (App "nativeLtDouble" e) =
              e <> "<"
          alg (App e1 e2) | "div(" `isPrefixOf` e1 =
              e1 <> e2 <> ")"
          alg (App e1 e2) = "(" <> mapOp e1 <> " (" <> e2 <> "))"
          alg (Let s e1 e2) =
            "const " <> s <> " = " <>
            (if "const" `isPrefixOf` e1
                then "function () { " <> e1 <> " }();" else e1) <>
            (if e2 == "()" then "" else
                (if isPrefixOf "if" e2 || isPrefixOf "const" e2 then "; " else "; return ") <> e2)
          alg (IfThenElse p e1 e2) =
                 "function () { if (" <> p <> ") { " <>
                     (if isPrefixOf "if" e1 || isPrefixOf "const" e1 then e1 else "return " <> e1)  <>
                     " } else { " <> (if isPrefixOf "if" e2 || isPrefixOf "const" e2 then e2 else "return " <> e2) <> " } }()"
          alg (AppClosure "nativeInt" e2) = e2
          alg (AppClosure "nativeDouble" e2) = e2
          alg (AppClosure e1 e2) =
              let e2' = if isPrefixOf "if" e2 || isPrefixOf "const" e2 then "(function(){ " <> e2 <> " })()" else e2 in
              "applyClosure("<> mapOp e1 <> "," <> e2' <> ")"
          alg (GetEnv name a) = "getEnv(" <> "\"" <> pack name <> "\"" <> "," <> a <> ")"
          alg (SetEnv name v b) = "setEnv(" <> "\"" <> pack name <> "\"" <> "," <> "(function(){ return " <> v <> "; })" <> "," <> b <> ")"
          alg (MkClosure name) = "mkClosure(" <> pack name <> ")"
          alg x = error (show x)

generateDecl :: TypedExp -> Text
generateDecl = generateLet . unwrap
  where generateLet (In (Defn _ n v)) =
            let s = generateExp n in
            let e1 = generateExp v in
            "const " <> s <> " = " <>
            (if "const" `isPrefixOf` e1
                then "function () { " <> e1 <> " }();" else e1)
        generateLet _ = undefined

generate :: [TypedExp] -> Text
generate es = intercalate "\n" (generateDecl <$> es)

generateData :: [TypeDecl] -> Text
generateData = foldr (<>) "" . (generateJsForDataDecl <$>)

generateJsForDataDecl :: TypeDecl -> Text
generateJsForDataDecl (TypeDecl t ts) = 
    foldr (<>) "" (generateJsForConstr t <$> ts)

generateJsForConstr :: Type -> Type -> Text
generateJsForConstr _ (TyCon n) = pack [i|
    class __#{n} {
      constructor() {}
    }

    const #{n} = new __#{n}();

    const is#{n} = mkClosure(function([_, x]) {
        return x instanceof __#{n};
    })
|]
generateJsForConstr _ (TyApp (TyCon n) _) = pack [i|
    class __#{n} {
      constructor(value) {
        this.value = value;
      }
    }

    const #{n} = mkClosure(function([_, x]) {
        return new __#{n}(x);
    })

    const is#{n} = mkClosure(function([_, x]) {
        return x instanceof __#{n};
    })

    const extract#{n} = mkClosure(function ([_, x]){
        return x.value;
    })
|]
generateJsForConstr _ (TyApp (TyApp (TyCon n) _)_) = pack [i|
    class __#{n} {
      constructor(value1, value2) {
        this.value1 = value1;
        this.value2 = value2;
      }
    }

    const #{n} = mkClosure(function([_, x]) {
        return setEnv("x", x, mkClosure(function([env, y]){
            return new __#{n}(env["x"], y);
        })) 
    })

    const is#{n} = mkClosure(function([_, x]) {
        return x instanceof __#{n};
    })

    const extract#{n} = mkClosure(function ([_, x]){
        return [x.value1. x.value2];
    })
|]
generateJsForConstr _ _ = error "Unknown constructor shape, unable to generate JS"
