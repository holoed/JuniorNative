{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Operators
import Primitives
import PAst

import Control.Monad.Except

}

-- Entry point
%name expr

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { TokenLet $$ }
    true  { TokenTrue $$ }
    false { TokenFalse $$ }
    if    { TokenIf $$ }
    then  { TokenThen $$ }
    else  { TokenElse $$ }
    in    { TokenIn $$ }
    NUM   { TokenNum $$ }
    STRING { TokenString $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda $$ }
    '->'  { TokenArrow $$ }
    '='   { TokenEq $$ }
    '=='  { TokenEql $$ }
    '&&'  { TokenAnd $$ }
    '||'  { TokenOr $$ }
    '++'  { TokenConcat $$ }
    '+'   { TokenAdd $$ }
    '-'   { TokenSub $$ }
    '*'   { TokenMul $$ }
    '/'   { TokenDiv $$ }
    '>'   { TokenGt  $$ }
    '<'   { TokenLt  $$ }
    ','   { TokenComma }
    '('   { TokenLParen $$ }
    ')'   { TokenRParen }
    '.'   { TokenDot $$ }
    '[]'  { TokenEmpty $$ }

-- Operators
%right '||'
%right '&&'
%nonassoc '>' '<'
%left '=='
%left '++'
%left '+' '-'
%left '*' '/'
%right '.'
%%

Decls : Expr                       { [$1] }
      | Decl                       { [$1] }   
      | Decl Decls                 { $1 : $2 }

Decl : let Pats '=' Expr           { defn (mkLoc $1) $2 $4 }

Expr : let Pats '=' Expr in Expr   { leT (mkLoc $1) $2 $4 $6 }
     | '\\' Pats '->' Expr         { lam (mkLoc $1) $2 $4 }
     | if Expr then Expr else Expr { ifThenElse (mkLoc $1) $2 $4 $6 }
     | Form                        { $1 }

Form : Form '+' Form               { infixApp (mkLoc $2) plusOp $1 $3 }
     | Form '-' Form               { infixApp (mkLoc $2) subOp $1 $3 }
     | Form '*' Form               { infixApp (mkLoc $2) mulOp $1 $3 }
     | Form '/' Form               { infixApp (mkLoc $2) divOp $1 $3 }
     | Form '&&' Form              { infixApp (mkLoc $2) andOp $1 $3 }
     | Form '||' Form              { infixApp (mkLoc $2) orOp $1 $3 }
     | Form '==' Form              { infixApp (mkLoc $2) eqeqOp $1 $3 }
     | Form '>' Form               { infixApp (mkLoc $2) gtOp $1 $3 }
     | Form '<' Form               { infixApp (mkLoc $2) ltOp $1 $3 }
     | Form '++' Form              { infixApp (mkLoc $2) plusplusOp $1 $3}
     | Form '.' Form               { infixApp (mkLoc $2) dotOp $1 $3}
     | Fact                        { $1 }

Fact : Fact Atom                   { infixApp (mkLoc alexStartPos) juxtaOp $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | '(' Exprs ')'               { mkTuple (mkLoc $1) $2 }
     | NUM                         { lit (mkLoc (fst $1)) (snd $1) }
     | STRING                      { lit (mkLoc (fst $1)) (snd $1) }
     | VAR                         { var (mkLoc (fst $1)) (snd $1) }
     | true                        { lit (mkLoc $1) (B True) }
     | false                       { lit (mkLoc $1) (B False) }
     | '[]'                        { var (mkLoc $1) "[]" }

Exprs : Expr                       { [$1] }
      | Expr ',' Exprs             { $1 : $3 }

Pat  : '(' PatList ')'             { tuplePat (mkLoc $1) $2 }
     | '(' '++' ')'                { varPat (mkLoc $2) "++" } -- Find better way to handle operators
     | VAR                         { varPat (mkLoc (fst $1)) (snd $1) }
                       

PatList : Pat                       { [$1] }
        | Pat ',' PatList           { $1 : $3 }

Pats : Pat                         { [$1] }
     | Pat Pats                    { $1 : $2 }

{

mkLoc :: AlexPosn -> Loc
mkLoc (AlexPn x y z) = Loc x y z

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String [SynExp]
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
