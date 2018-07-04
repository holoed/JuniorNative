{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Ast

import Control.Monad.Except

}

-- Entry point
%name expr

-- Entry point
%name expr

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { TokenLet }
    true  { TokenTrue }
    false { TokenFalse }
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '=='   { TokenEql }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '/'   { TokenDiv }
    ','   { TokenComma }
    '('   { TokenLParen }
    ')'   { TokenRParen }

-- Operators
%left '=='
%left '+' '-'
%left '*' '/'
%%

Expr : let VAR '=' Expr in Expr    { leT $2 $4 $6 }
     | '\\' VAR '->' Expr          { lam $2 $4 }
     | if Expr then Expr else Expr { ifThenElse $2 $4 $6 }
     | Form                        { $1 }

Form : Form '+' Form               { app (app (var "+") $1) $3 }
     | Form '-' Form               { app (app (var "-") $1) $3 }
     | Form '*' Form               { app (app (var "*") $1) $3 }
     | Form '/' Form               { app (app (var "/") $1) $3 }
     | Form '==' Form              { app (app (var "==") $1) $3 }
     | Fact                        { $1 }

Fact : Fact Atom                   { app $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | '(' Exprs ')'               { mkTuple $2 }
     | NUM                         { lit $1 }
     | VAR                         { var $1 }
     | true                        { lit (B True) }
     | false                       { lit (B False) }

Exprs : Expr                       { [$1] }
      | Expr ',' Exprs             { $1 : $3 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Exp
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens

}
