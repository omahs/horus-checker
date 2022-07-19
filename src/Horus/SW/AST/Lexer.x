{
{-# OPTIONS_GHC -w #-}
module Horus.SW.AST.Lexer where

import Data.Text (Text, pack)
import Text.Read (read)

import Horus.Util (parseHexInteger)

}

%wrapper "basic"

tokens :-
    $white+                     ;
    \,                          { \s -> TokenComma }
    \*                          { \s -> TokenStar }
    \*\*                        { \s -> TokenDoubleStar }
    [0-9]+                      { \s -> TokenInteger (read s)}
    0x[0-9]+                    { \s -> case parseHexInteger s of 
                                            Just x -> TokenInteger x 
                                            Nothing -> error $ "Can't parse" <> s <> " as a hex number." }
    new                         { \s -> TokenNew }
    cast                        { \s -> TokenCast }
    felt                        { \s -> TokenFelt }
    codeoffset                  { \s -> TokenCodeoffset }
    ap                          { \s -> TokenAp }
    fp                          { \s -> TokenFp }
    [a-zA-Z\_][a-zA-Z\_0-9]*    { \s -> TokenIdentifier (pack s) }
    \.                          { \s -> TokenDot }
    ":"                         { \s -> TokenColon }
    \(                          { \s -> TokenLParen }
    \)                          { \s -> TokenRParen }
    \[                          { \s -> TokenLBracket }
    \]                          { \s -> TokenRBracket }
    "+"                         { \s -> TokenPlus }
    "-"                         { \s -> TokenMinus}
    "/"                         { \s -> TokenDiv }
    "&"                         { \s -> TokenAddrOf }


{
data Token
    = TokenDot
    | TokenComma
    | TokenColon
    | TokenStar
    | TokenDoubleStar
    | TokenLParen
    | TokenRParen
    | TokenLBracket
    | TokenRBracket
    | TokenFelt
    | TokenCodeoffset
    | TokenPlus
    | TokenMinus 
    | TokenDiv
    | TokenAddrOf
    | TokenNew
    | TokenCast
    | TokenAp
    | TokenFp
    | TokenInteger Integer
    | TokenIdentifier Text
    deriving (Show)
}
