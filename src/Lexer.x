{
{-# LANGUAGE FlexibleContexts #-}
module Lexer (
  Token(..),
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
  let                           {\p s -> TokenLet }
  True                          {\p s -> TokenTrue }
  False                         {\p s -> TokenFalse }
  if                            {\p s -> TokenIf }
  then                          {\p s -> TokenThen }
  else                          {\p s -> TokenElse }
  in                            {\p s -> TokenIn }
  $digit+                       {\p s -> TokenNum (I $ read s) }
  @string                       {\p s -> TokenString (S s) }
  "->"                          {\p s -> TokenArrow }
  "=="                          {\p s -> TokenEql }
  \=                            {\p s -> TokenEq }
  \\                            {\p s -> TokenLambda }
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
  $alpha [$alpha $digit \_ \']* {\p s -> TokenSym s }

{

data Token
  = TokenLet
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenIn
  | TokenLambda
  | TokenTrue
  | TokenFalse
  | TokenNum Prim
  | TokenString Prim
  | TokenSym String
  | TokenArrow
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
