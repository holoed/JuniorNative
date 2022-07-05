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
$whitechar = [ \t\n\r\f\v]
@gap     = \\ $whitechar+ \\
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc)
@string = \" ($printable # [\"\\] | " " | @escape | @gap)* \"
@char = \' ($printable # \') \'

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "--".*                         ;

  -- Syntax
  match                         {\p s -> TokenMatch (p, s) }
  with                          {\p s -> TokenWith (p, s) }
  deriving                      {\p s -> TokenDeriving (p, s) }
  data                          {\p s -> TokenData (p, s) }
  val                           {\p s -> TokenVal (p, s) }
  let                           {\p s -> TokenLet (p, s) }
  True                          {\p s -> TokenTrue (p, s) }
  False                         {\p s -> TokenFalse (p, s) }
  if                            {\p s -> TokenIf (p, s) }
  then                          {\p s -> TokenThen (p, s) }
  else                          {\p s -> TokenElse (p, s) }
  in                            {\p s -> TokenIn (p, s) }
  @sign? @number                {\p s -> TokenNum (p, strToPrim s) }
  @string                       {\p s -> TokenString (p, (S (tail (init s)))) }
  @char                         {\p s -> TokenChar (p, ((C . (!!1)) s)) }
  "->"                          {\p s -> TokenArrow (p, s) }
  "=>"                          {\p s -> TokenFatArrow (p, s) }
  "<*>"                         {\p s -> TokenLtStarGt (p, s) }
  "<$>"                         {\p s -> TokenLtDollarGt (p, s) }
  "<>"                          {\p s -> TokenLtGt (p, s) }
  ">=>"                         {\p s -> TokenGtEqGt (p, s) }
  ">>="                         {\p s -> TokenGtGtEq (p, s) }
  "=="                          {\p s -> TokenEql (p, s) }
  "/="                          {\p s -> TokenNotEql (p, s) }
  ">="                          {\p s -> TokenGtEql (p, s) }
  "<="                          {\p s -> TokenLtEql (p, s) }
  \=                            {\p s -> TokenEq (p, s) }
  \\                            {\p s -> TokenLambda (p, s) }
  "[]"                          {\p s -> TokenEmpty (p, s) }
  "()"                          {\p s -> TokenUnit (p, s) }
  ":"                           {\p s -> TokenCons (p, s) }
  "::"                          {\p s -> TokenColonColon (p, s) }
  "++"                          {\p s -> TokenConcat (p, s) }
  "!!"                          {\p s -> TokenExclExclMark (p, s) }
  "&&"                          {\p s -> TokenAnd (p, s) }
  "||"                          {\p s -> TokenOr (p, s) }
  [\|]                          {\p s -> TokenVBar (p, s) }
  [\+]                          {\p s -> TokenAdd (p, s) }
  [\-]                          {\p s -> TokenSub (p, s) }
  [\*]                          {\p s -> TokenMul (p, s) }
  [\/]                          {\p s -> TokenDiv (p, s) }
  [\>]                          {\p s -> TokenGt (p, s)  }
  [\<]                          {\p s -> TokenLt (p, s)  }
  \(                            {\p s -> TokenLParen (p, s) }
  \)                            {\p s -> TokenRParen (p, s) }
  \[                            {\p s -> TokenLBracket (p, s) }
  \]                            {\p s -> TokenRBracket (p, s) }
  ","                           {\_ _ -> TokenComma }
  "."                           {\p s -> TokenDot (p, s) }
  [A-Z]  [$alpha $digit \_ \']* {\p s -> TokenUSym (p, s) }
  $alpha [$alpha $digit \_ \']* {\p s -> TokenSym (p, s) }

{

data Token
  = TokenMatch (AlexPosn, String)
  | TokenWith (AlexPosn, String)
  | TokenDeriving (AlexPosn, String)
  | TokenData (AlexPosn, String)
  | TokenVal (AlexPosn, String)
  | TokenLet (AlexPosn, String)
  | TokenIf (AlexPosn, String)
  | TokenThen (AlexPosn, String)
  | TokenElse (AlexPosn, String)
  | TokenIn (AlexPosn, String)
  | TokenLambda (AlexPosn, String)
  | TokenTrue (AlexPosn, String)
  | TokenFalse (AlexPosn, String)
  | TokenNum (AlexPosn, Prim)
  | TokenString (AlexPosn, Prim)
  | TokenChar (AlexPosn, Prim)
  | TokenSym (AlexPosn, String)
  | TokenUSym (AlexPosn, String)
  | TokenArrow (AlexPosn, String)
  | TokenFatArrow (AlexPosn, String)
  | TokenLtStarGt (AlexPosn, String)
  | TokenLtDollarGt (AlexPosn, String)
  | TokenLtGt (AlexPosn, String)
  | TokenGtEqGt (AlexPosn, String)
  | TokenGtGtEq (AlexPosn, String)
  | TokenConcat (AlexPosn, String)
  | TokenExclExclMark (AlexPosn, String)
  | TokenEmpty (AlexPosn, String)
  | TokenCons (AlexPosn, String)
  | TokenColonColon (AlexPosn, String)
  | TokenAnd (AlexPosn, String)
  | TokenOr (AlexPosn, String)
  | TokenEq (AlexPosn, String)
  | TokenEql (AlexPosn, String)
  | TokenNotEql (AlexPosn, String)
  | TokenGtEql (AlexPosn, String)
  | TokenLtEql (AlexPosn, String)
  | TokenAdd (AlexPosn, String)
  | TokenSub (AlexPosn, String)
  | TokenMul (AlexPosn, String)
  | TokenDiv (AlexPosn, String)
  | TokenGt (AlexPosn, String)
  | TokenLt (AlexPosn, String)
  | TokenLParen (AlexPosn, String)
  | TokenRParen (AlexPosn, String)
  | TokenLBracket (AlexPosn, String)
  | TokenRBracket (AlexPosn, String)
  | TokenComma
  | TokenDot (AlexPosn, String)
  | TokenUnit (AlexPosn, String)
  | TokenVBar (AlexPosn, String)
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
