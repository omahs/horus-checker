{
module Horus.SMT2.Parser (parseAssertion) where

import Data.Map (Map)
import Data.SBV (SBool, (.==), SInteger, (.<), sInteger, uninterpret)
import Data.Text (Text, unpack)
import qualified Data.Map as Map (empty, (!), insert)

import Horus.SMT2.Lexer (Token (..))

}

%name parseAssertion_
%tokentype { Token }
%error { parseError }

%token
  '('    { OpenParT }
  ')'    { ClosParT }
  '='    { EqT }
  '<'    { LtT }
  '+'    { PlusT }
  '-'    { MinusT }
  '*'    { StarT }
  let    { LetT }
  memory { MemoryT }
  predef { PredefVarT $$ }
  var    { VarT $$ }
  int    { IntT $$ }

%%

BoolE :: { Env -> SBool }
  : var                                           { (Map.! $1) . e_boolLocals}
  | '(' BoolApp ')'                               { $2 }
  | '(' let '(' Bindings ')' BoolE ')'            { \e -> $6 (addBindings ($4 e) e) }

BoolApp :: { Env -> SBool }
  : '=' ArithE ArithE                             { \e -> $2 e .== $3 e }
  | '<' ArithE ArithE                             { \e -> $2 e .< $3 e }

ArithE :: { Env -> SInteger }
  : int                                           { const (fromInteger $1) }
  | predef                                        { const (uninterpret (unpack $1)) }
  | var                                           { (Map.! $1) . e_arithLocals }
  | '(' ArithApp ')'                              { $2 }

ArithApp :: { Env -> SInteger }
  : memory ArithE                                 { \e -> memory ($2 e) }
  | '+' ArithE ArithE                             { \e -> $2 e + $3 e }
  | '-' ArithE ArithE                             { \e -> $2 e - $3 e }
  | '*' ArithE ArithE                             { \e -> $2 e * $3 e }


Bindings :: { Env -> [(Text, Value)] }
  : '(' rev_list1(Binding) ')'                    { \e -> map ($ e) $2 }

Binding :: { Env -> (Text, Value) }
  : '(' var ArithE ')'                            { \e -> ($2, VArith ($3 e)) }
  | '(' var BoolE ')'                             { \e -> ($2, VBool ($3 e)) }


rev_list1(p)
  : p                                             { [$1] }
  | rev_list1(p) p                                { $2 : $1 }
{

parseError :: [Token] -> a
parseError ts = error ("Parse error:" <> concat [" " <> show t | t <- ts])

data Env = Env
  { e_arithLocals :: Map Text SInteger
  , e_boolLocals :: Map Text SBool
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

parseAssertion :: [Token] -> SBool
parseAssertion = flip parseAssertion_ emptyEnv

data Value = VBool SBool | VArith SInteger

addBindings :: [(Text, Value)] -> Env -> Env
addBindings = flip (foldr folder)
  where
    folder (k, VBool b) env = env{ e_boolLocals = Map.insert k b (e_boolLocals env) }
    folder (k, VArith a) env = env{ e_arithLocals = Map.insert k a (e_arithLocals env) }

memory :: SInteger -> SInteger
memory = uninterpret "memory"

}
