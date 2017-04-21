{
module Parser where

import Lexer
import Ast
}

%name hslam
%tokentype { Token }
%error { parseError }
%token
  "\\"          { TBackslash }
  identifier    { TIdentifier $$ }
  "."           { TDot }
  "let"         { TLet }
  "="           { TEquals }
  "in"          { TIn }
  "+"           { TPlus }
  "*"           { TMultiply }
  "("           { TLParen }
  ")"           { TRParen }
  integer       { TInteger $$ }
%%

expression:
  "\\" identifier "." expression
                { Abstraction $2 $4 }
  | "let" identifier "=" expression "in" expression
                { Application (Abstraction $2 $6) $4 }
  | plus_expr   { $1 }

plus_expr:
  times_expr "+" plus_expr { Binop Plus $1 $3 }
  | times_expr        { $1 }

times_expr:
  apply_expr "*" times_expr { Binop Times $1 $3 }
  | apply_expr        { $1 }

apply_expr:
  apply_expr simple_expr    { Application $1 $2 }
  | simple_expr       { $1 }

simple_expr:
  "(" expression ")"  { $2 }
  | identifier       { Var $1 }
  | integer          { Integer $1 }

{
parseError e = error $ show e
}
