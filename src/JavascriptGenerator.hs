{-# LANGUAGE OverloadedStrings #-}
module JavascriptGenerator where

import Ast ( ExpF(Lit, Var, VarPat, Lam, App, Let, IfThenElse, MkTuple) )
import TypedAst ( TypedExp )
import Data.Text (Text, intercalate, pack, isPrefixOf)
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
mapOp ":"  = "__colon"
mapOp "."  = "__dot"
mapOp x    = x

generateExp :: Fix ExpF -> Text
generateExp = cataRec alg 
    where alg (Lit p) = generatePrim p
          alg (Var n) = pack n
          alg (VarPat n) = pack n
          alg (Lam s e) = "function (" <> s <> ") { " <> (if isPrefixOf "if" e || isPrefixOf "var" e then "" else " return ") <> e <> " }"
          alg (App e1 e2) = "(" <> mapOp e1 <> " (" <> e2 <> "))"
          alg (Let s e1 e2) =
            "var " <> s <> " = " <>
            (if "var" `isPrefixOf` e1
                then "function () { " <> e1 <> " }();" else e1) <>
            (if e2 == "()" then "" else
                (if isPrefixOf "if" e2 || isPrefixOf "var" e2 then "; " else "; return ") <> e2)
          alg (IfThenElse p e1 e2) =
                 "function() { if (" <> p <> ") { " <> 
                     (if isPrefixOf "if" e1 || isPrefixOf "var" e1 then e1 else "return " <> e1)  <> 
                     " } else { " <> (if isPrefixOf "if" e2 || isPrefixOf "var" e2 then e2 else "return " <> e2) <> " } }()"
          alg (MkTuple xs) = pack "[" <> intercalate (pack ",") xs <> pack "]" 
          alg e = error ("Unsupported: " ++ show e)

generateDecl :: TypedExp -> Text
generateDecl = generateLet . unwrap
  where generateLet (In (Let n v _)) =
            let s = generateExp n in
            let e1 = generateExp v in 
            "var " <> s <> " = " <>
            (if "var" `isPrefixOf` e1
                then "function () { " <> e1 <> " }();" else e1) 
        generateLet _ = undefined

generate :: [TypedExp] -> Text
generate es = intercalate "\n" (generateDecl <$> es)