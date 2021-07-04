{
{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  Token(..),
  AlexPosn(..),
  alexStartPos,
  scanTokens,
  mkLoc
) where

import Location (Loc(..), PString(..))
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
  let                           {\p s -> TokenLet (p, s) }
  True                          {\p s -> TokenTrue (p, s) }
  False                         {\p s -> TokenFalse (p, s) }
  if                            {\p s -> TokenIf (p, s) }
  then                          {\p s -> TokenThen (p, s) }
  else                          {\p s -> TokenElse (p, s) }
  in                            {\p s -> TokenIn (p, s) }
  @sign? @number                {\p s -> TokenNum (p, strToPrim s) }
  @string                       {\p s -> TokenString (p, (S s)) }
  "->"                          {\p s -> TokenArrow (p, s) }
  "=="                          {\p s -> TokenEql (p, s) }
  \=                            {\p s -> TokenEq (p, s) }
  \\                            {\p s -> TokenLambda (p, s) }
  "[]"                          {\p s -> TokenEmpty (p, s) }
  ":"                           {\p s -> TokenCons (p, s) }
  "++"                          {\p s -> TokenConcat (p, s) }
  "&&"                          {\p s -> TokenAnd (p, s) }
  "||"                          {\p s -> TokenOr (p, s) }
  [\+]                          {\p s -> TokenAdd (p, s) }
  [\-]                          {\p s -> TokenSub (p, s) }
  [\*]                          {\p s -> TokenMul (p, s) }
  [\/]                          {\p s -> TokenDiv (p, s) }
  [\>]                          {\p s -> TokenGt (p, s)  }
  [\<]                          {\p s -> TokenLt (p, s)  }
  \(                            {\p s -> TokenLParen (p, s) }
  \)                            {\p s -> TokenRParen (p, s) }
  ","                           {\p s -> TokenComma }
  "."                           {\p s -> TokenDot (p, s) }
  $alpha [$alpha $digit \_ \']* {\p s -> TokenSym (p, s) }

{

data Token
  = TokenLet (AlexPosn, String)
  | TokenIf (AlexPosn, String)
  | TokenThen (AlexPosn, String)
  | TokenElse (AlexPosn, String)
  | TokenIn (AlexPosn, String)
  | TokenLambda (AlexPosn, String)
  | TokenTrue (AlexPosn, String)
  | TokenFalse (AlexPosn, String)
  | TokenNum (AlexPosn, Prim)
  | TokenString (AlexPosn, Prim)
  | TokenSym (AlexPosn, String)
  | TokenArrow (AlexPosn, String)
  | TokenConcat (AlexPosn, String)
  | TokenEmpty (AlexPosn, String)
  | TokenCons (AlexPosn, String)
  | TokenAnd (AlexPosn, String)
  | TokenOr (AlexPosn, String)
  | TokenEq (AlexPosn, String)
  | TokenEql (AlexPosn, String)
  | TokenAdd (AlexPosn, String)
  | TokenSub (AlexPosn, String)
  | TokenMul (AlexPosn, String)
  | TokenDiv (AlexPosn, String)
  | TokenGt (AlexPosn, String)
  | TokenLt (AlexPosn, String)
  | TokenLParen (AlexPosn, String)
  | TokenRParen (AlexPosn, String)
  | TokenComma
  | TokenDot (AlexPosn, String)
  | TokenEOF
  deriving (Eq,Show)

mkLoc :: (AlexPosn, String) -> Loc
mkLoc (AlexPn _ y z, s) = Loc (length s) y z

scanTokens :: String -> Except PString [Token]
scanTokens str = go (alexStartPos, '\n',[],str) where
  go inp@(pos, _, _bs, str') =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError (p,_,_,s) -> throwError $ PStr ("lexical error", Just $ mkLoc (p, s))
     AlexSkip  inp' _     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act pos (take len str')
      return (rest : res)

}
