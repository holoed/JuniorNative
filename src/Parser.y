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
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    STRING { TokenString $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '=='  { TokenEql }
    '++'  { TokenConcat }
    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '/'   { TokenDiv }
    '>'   { TokenGt }
    '<'   { TokenLt }
    ','   { TokenComma }
    '('   { TokenLParen }
    ')'   { TokenRParen }

-- Operators
%nonassoc '>' '<'
%left '=='
%left '+' '-'
%left '*' '/'
%left '++'
%%

Decls : Expr                       { [$1] }
      | Decl                       { [$1] }   
      | Decl Decls                 { $1 : $2 }

Decl : let Vars '=' Expr           { defn (mkPos $1) $2 $4 }

Expr : let Vars '=' Expr in Expr   { leT $2 $4 $6 }
     | '\\' Vars '->' Expr         { lam $2 $4 }
     | if Expr then Expr else Expr { ifThenElse $2 $4 $6 }
     | Form                        { $1 }

Form : Form '+' Form               { infixApp plusOp $1 $3 }
     | Form '-' Form               { infixApp subOp $1 $3 }
     | Form '*' Form               { infixApp mulOp $1 $3 }
     | Form '/' Form               { infixApp divOp $1 $3 }
     | Form '==' Form              { infixApp eqeqOp $1 $3 }
     | Form '>' Form               { infixApp gtOp $1 $3 }
     | Form '<' Form               { infixApp ltOp $1 $3 }
     | Form '++' Form              { infixApp plusplusOp $1 $3}
     | Fact                        { $1 }

Fact : Fact Atom                   { infixApp juxtaOp $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | '(' Exprs ')'               { mkTuple $2 }
     | NUM                         { lit (mkPos (fst $1)) (snd $1) }
     | STRING                      { lit (mkPos (fst $1)) (snd $1) }
     | VAR                         { var $1 }
     | true                        { lit (mkPos $1) (B True) }
     | false                       { lit (mkPos $1) (B False) }

Exprs : Expr                       { [$1] }
      | Expr ',' Exprs             { $1 : $3 }

Vars : VAR                         { [$1] }
     | VAR Vars                    { $1 : $2 }

{

mkPos :: AlexPosn -> Pos
mkPos (AlexPn x y z) = Pos x y z

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
