{
{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  Token(..),
  AlexPosn(..),
  alexStartPos,
  scanTokens
) where

import PAst ()
import Primitives

import Control.Monad.Except

}

%wrapper "posn"

$digit = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@number      = @decimal
             | @decimal \. @decimal @exponent?
             | @decimal @exponent
             | 0[oO] @octal
             | 0[xX] @hexadecimal


$alpha = [a-zA-Z]
$eol   = [\n]
$graphic = $printable # $white
@string = \" ($graphic # \")* \"

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  let                           {\p s -> TokenLet p }
  True                          {\p s -> TokenTrue p }
  False                         {\p s -> TokenFalse p }
  if                            {\p s -> TokenIf p }
  then                          {\p s -> TokenThen p }
  else                          {\p s -> TokenElse p }
  in                            {\p s -> TokenIn p }
  @sign? @number                {\p s -> TokenNum (p, strToPrim s) }
  @string                       {\p s -> TokenString (p, (S s)) }
  "->"                          {\p s -> TokenArrow p }
  "=="                          {\p s -> TokenEql p }
  \=                            {\p s -> TokenEq p }
  \\                            {\p s -> TokenLambda p }
  "[]"                          {\p s -> TokenEmpty p }
  ":"                           {\p s -> TokenCons p }
  "++"                          {\p s -> TokenConcat p }
  "&&"                          {\p s -> TokenAnd p }
  "||"                          {\p s -> TokenOr p }
  [\+]                          {\p s -> TokenAdd p }
  [\-]                          {\p s -> TokenSub p }
  [\*]                          {\p s -> TokenMul p }
  [\/]                          {\p s -> TokenDiv p }
  [\>]                          {\p s -> TokenGt p  }
  [\<]                          {\p s -> TokenLt p  }
  \(                            {\p s -> TokenLParen p }
  \)                            {\p s -> TokenRParen }
  ","                           {\p s -> TokenComma }
  "."                           {\p s -> TokenDot p }
  $alpha [$alpha $digit \_ \']* {\p s -> TokenSym (p, s) }

{

data Token
  = TokenLet AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenIn AlexPosn
  | TokenLambda AlexPosn
  | TokenTrue AlexPosn
  | TokenFalse AlexPosn
  | TokenNum (AlexPosn, Prim)
  | TokenString (AlexPosn, Prim)
  | TokenSym (AlexPosn, String)
  | TokenArrow AlexPosn
  | TokenConcat AlexPosn
  | TokenEmpty AlexPosn
  | TokenCons AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenEq AlexPosn
  | TokenEql AlexPosn
  | TokenAdd AlexPosn
  | TokenSub AlexPosn
  | TokenMul AlexPosn
  | TokenDiv AlexPosn
  | TokenGt AlexPosn
  | TokenLt AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen
  | TokenComma
  | TokenDot AlexPosn
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go (alexStartPos, '\n',[],str) where
  go inp@(pos, _,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act pos (take len str)
      return (rest : res)

}
