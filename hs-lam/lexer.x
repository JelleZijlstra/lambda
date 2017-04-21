{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+       ;
  "("           { \s -> TLParen }
  ")"           { \s -> TRParen }
  "\"          { \s -> TBackslash }
  "."           { \s -> TDot }
  "+"           { \s -> TPlus }
  "*"           { \s -> TMultiply }
  "let"         { \s -> TLet }
  "in"          { \s -> TIn }
  "="           { \s -> TEquals }
  $alpha[$alpha $digit \_ \']*  { \s -> TIdentifier s }
  $digit+       { \s -> TInteger $ read s }
  "#" .*        ;
{

data Token =
    TLParen |
    TRParen |
    TBackslash |
    TDot |
    TIdentifier String |
    TInteger Int |
    TPlus |
    TMultiply |
    TLet |
    TIn |
    TEquals
    deriving (Eq, Show)
}
