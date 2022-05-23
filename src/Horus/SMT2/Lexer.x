{
module Horus.SMT2.Lexer (Token (..), alexScanTokens) where

import Data.Text (Text, pack)

}

%wrapper "basic"

$digit = 0-9
$sym = [a-zA-Z!]

tokens :-
  $white+               ;
  \(                    { const OpenParT }
  \)                    { const ClosParT }
  =                     { const EqT }
  \<                    { const LtT }
  \+                    { const PlusT }
  \-                    { const MinusT }
  \*                    { const StarT }
  let                   { const LetT }
  memory                { const MemoryT }
  (ap | fp | prime)     { PredefVarT . pack }
  $sym [$sym $digit]*   { VarT . pack }
  $digit+               { IntT . read }

{

data Token
  = OpenParT
  | ClosParT
  | EqT
  | LtT
  | PlusT
  | MinusT
  | StarT
  | LetT
  | MemoryT
  | PredefVarT Text
  | VarT Text
  | IntT Integer
  deriving (Eq, Show)
}
