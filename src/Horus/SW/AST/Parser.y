{
{-# OPTIONS_GHC -w #-}
module Horus.SW.AST.Parser where

import Data.Text (pack)

import Horus.SW.AST (CairoType (..), Expr (..), NewOpData (..))
import Horus.SW.AST.Lexer (Token (..))
import Horus.SW.Instruction (PointerRegister (..))
import Horus.SW.ScopedName (ScopedName (..))


}

%tokentype { Token }
%error { happyError }

%name parseCairoType Type
%name parseCairoExpr Expr

%token
    '.'          {TokenDot}
    ','          {TokenComma}
    ':'          {TokenColon}
    '*'          {TokenStar}
    '**'         {TokenDoubleStar}
    '('          {TokenLParen}
    ')'          {TokenRParen}
    '+'          {TokenPlus}
    '-'          {TokenMinus}
    '/'          {TokenDiv}
    '&'          {TokenAddrOf}
    '['          {TokenLBracket}
    ']'          {TokenRBracket}
    new          {TokenNew}
    cast         {TokenCast}
    ap           {TokenAp}
    fp           {TokenFp}
    felt         {TokenFelt}
    codeoffset   {TokenCodeoffset}
    identifier   {TokenIdentifier $$}
    int          {TokenInteger $$}

%%

Identifier : identifier                 { ScopedName [$1] }
           | Identifier '.' identifier  { $1 <> (ScopedName [$3]) }

NamedType : NonIdentifierType       { (Nothing, Just $1) }
          | Identifier              { (Just $1, Nothing) }
          | Identifier ':' Type     { (Just $1, Just $3) }

CommaTypes : NamedType                {[$1]}
           | CommaTypes ',' NamedType { $1 ++ [$3]}

NonIdentifierType   : felt               { TypeFelt }
                    | codeoffset         { TypeCodeoffset }
                    | Type '*'           { TypePointer $1 }
                    | Type '**'          { TypePointer (TypePointer $1)}
                    | '(' CommaTypes ')' { TypeTuple $2 }

Type : NonIdentifierType    { $1 }
     | Identifier           { TypeStruct $1 }

Expr : Sum { $1 }

Sum : Product { $1 }
    | Sum '+' Product { Sum $1 $3 }
    | Sum '-' Product { Sub $1 $3 }

Product : Unary { $1 }
        | Product '*' Unary { Prod $1 $3 }
        | Product '/' Unary { Div $1 $3 } 

Unary : Pow { $1 }
      | '&' Unary { AddrOf $2 }
      | '-' Unary { Neg $2 }
      | new Unary { New (NewOpData $2 True) }

Pow : Atom { $1 }
    | Atom '**' Pow { Pow ($1) $3 }

Atom : int                          { Const $1 }
     | Reg                          { Reg $1 }
     | Identifier                   { Identifier $1 }
     | '[' Expr ']'                 { Deref $2 }
     | Atom '[' Expr ']'            { Subscript $1 $3 }
     | cast '(' Expr ',' Type ')'   { Cast $3 $5 }
     | Atom '.' identifier          { Dot $1 $3 }
     | '(' ArgList ')'              { ParenOrTuple $2 }

ArgList : Expr                  { [$1] }
        | ArgList ',' Expr      { $1 ++ [$3] }

Reg : ap { AllocationPointer }
    | fp { FramePointer }

{

happyError :: [Token] -> a
happyError x = error ("Parse error " <> show x)

}
