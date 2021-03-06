{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Location (Loc, PString(..))
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
%monad { Except PString } { (>>=) } { return }
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
    ')'   { TokenRParen $$ }
    '.'   { TokenDot $$ }
    '[]'  { TokenEmpty $$ }
    ':'   { TokenCons $$ }

-- Operators
%right '||'
%right '&&'
%nonassoc '>' '<' '=='
%right '++' ':'
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
     | Form ':' Form               { infixApp (mkLoc $2) consOp $1 $3 }
     | Form '.' Form               { infixApp (mkLoc $2) dotOp $1 $3}
     | Fact                        { $1 }

Fact : Fact Atom                   { infixApp (mkLoc (alexStartPos, " ")) juxtaOp $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | '(' Exprs ')'               { mkTuple (mkLoc $1) $2 }
     | NUM                         { lit (mkLoc (fst $1, primToStr $ snd $1)) (snd $1) }
     | STRING                      { lit (mkLoc (fst $1, primToStr $ snd $1)) (snd $1) }
     | VAR                         { var (mkLoc ($1)) (snd $1) }
     | true                        { lit (mkLoc $1) (B True) }
     | false                       { lit (mkLoc $1) (B False) }
     | '[]'                        { var (mkLoc $1) "[]" }
     | '(' '+' ')'                 { var (mkLoc $2) "+" }
     | '(' '-' ')'                 { var (mkLoc $2) "-" }
     | '(' '*' ')'                 { var (mkLoc $2) "*" }
     | '(' '/' ')'                 { var (mkLoc $2) "/" }
     | '(' '++' ')'                { var (mkLoc $2) "++" }
     | '(' ':' ')'                 { var (mkLoc $2) ":" }

Exprs : Expr                       { [$1] }
      | Expr ',' Exprs             { $1 : $3 }

Pat  : '(' PatList ')'             { tuplePat (mkLoc $1) $2 }
     | '(' '++' ')'                { varPat (mkLoc $2) "++" }
     | '(' '.' ')'                 { varPat (mkLoc $2) "." }
     | VAR                         { varPat (mkLoc $1) (snd $1) }
                       

PatList : Pat                       { [$1] }
        | Pat ',' PatList           { $1 : $3 }

Pats : Pat                         { [$1] }
     | Pat Pats                    { $1 : $2 }

{

makePString :: Token -> PString
makePString (TokenLet (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenIf (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenThen (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenElse (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenIn (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenLambda (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenNum (p, n)) = PStr (show n, Just $ mkLoc (p, show n))
makePString (TokenLParen (p, n)) = PStr (n, Just $ mkLoc (p, n)) 
makePString (TokenRParen (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenAdd (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenMul (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenSub (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenDiv (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenDot (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenArrow (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenEq (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString (TokenEql (p, n)) = PStr (n, Just $ mkLoc (p, n))
makePString t = PStr (show t, Nothing) 
  
parseError :: [Token] -> Except PString a
parseError (l:ls) = 
  let (PStr (s, p)) = makePString l in
  throwError $ PStr ("Syntax error " ++ s, p)
parseError [] = throwError $ PStr ("Unexpected end of Input", Nothing)

parseExpr :: String -> Either PString [SynExp]
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either PString [Token]
parseTokens = runExcept . scanTokens

}
