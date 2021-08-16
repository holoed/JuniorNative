{-# LANGUAGE OverloadedStrings #-}
module JavascriptGenerator where

import Ast ( ExpF(Lit, Var, VarPat, Lam, App, Let, Defn, IfThenElse, MkTuple, TuplePat) )
import TypedAst ( TypedExp )
import Data.Text (Text, intercalate, pack, isPrefixOf, replace)
import RecursionSchemes ( cataRec )
import Primitives ( Prim(..) )
import Annotations (unwrap)
import Fixpoint ( Fix(In) )

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
mapOp x    = x

mapName :: Text -> Text
mapName "null" = "isEmpty"
mapName "eval" = "__eval"
mapName "mod"  = "__mod"
mapName "()" = "undefined"
mapName x      = replace "'" "Quoted" x

generateExp :: Fix ExpF -> Text
generateExp = cataRec alg
    where alg (Lit p) = generatePrim p
          alg (Var n) = mapName (pack n)
          alg (VarPat n) = mapName (pack n)
          alg (MkTuple xs) = pack "[" <> intercalate (pack ",") xs <> pack "]"
          alg (TuplePat xs) = pack "[" <> intercalate (pack ",") xs <> pack "]"
          alg (Lam s e) = "function (" <> s <> ") { " <> (if isPrefixOf "if" e || isPrefixOf "const" e then "" else " return ") <> e <> " }"
          alg (App e1 e2) = "(" <> mapOp e1 <> " (" <> e2 <> "))"
          alg (Let s e1 e2) =
            "const " <> s <> " = " <>
            (if "const" `isPrefixOf` e1
                then "function () { " <> e1 <> " }();" else e1) <>
            (if e2 == "()" then "" else
                (if isPrefixOf "if" e2 || isPrefixOf "const" e2 then "; " else "; return ") <> e2)
          alg (IfThenElse p e1 e2) =
                 "function() { if (" <> p <> ") { " <>
                     (if isPrefixOf "if" e1 || isPrefixOf "const" e1 then e1 else "return " <> e1)  <>
                     " } else { " <> (if isPrefixOf "if" e2 || isPrefixOf "const" e2 then e2 else "return " <> e2) <> " } }()"
          alg Defn {} = undefined

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