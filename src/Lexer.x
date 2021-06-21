{
{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  Token(..),
  AlexPosn(..),
  scanTokens
) where

import PAst
import Primitives

import Control.Monad.Except

}

%wrapper "posn"

$digit = 0-9
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
  $digit+                       {\p s -> TokenNum (p, (I $ read s)) }
  @string                       {\p s -> TokenString (p, (S s)) }
  "->"                          {\p s -> TokenArrow p }
  "=="                          {\p s -> TokenEql }
  \=                            {\p s -> TokenEq }
  \\                            {\p s -> TokenLambda p }
  "++"                          {\p s -> TokenConcat }
  [\+]                          {\p s -> TokenAdd }
  [\-]                          {\p s -> TokenSub }
  [\*]                          {\p s -> TokenMul }
  [\/]                          {\p s -> TokenDiv }
  [\>]                          {\p s -> TokenGt  }
  [\<]                          {\p s -> TokenLt  }
  \(                            {\p s -> TokenLParen }
  \)                            {\p s -> TokenRParen }
  ","                           {\p s -> TokenComma }
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
  | TokenConcat
  | TokenEq
  | TokenEql
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenGt
  | TokenLt
  | TokenLParen
  | TokenRParen
  | TokenComma
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
