module Parser(parseScheme) where

import Text.ParserCombinators.Parsec

import Ast

parseScheme :: String -> String -> Either ParseError Expr
parseScheme text file = parse schemeFile file text

schemeFile = do
    lst <- statementList
    eof
    return lst

statementList = do
    elements <- many $ list `returnLeft` ignored
    return $ StatementList elements

list = do
    elements <- between (char '(' `returnLeft` ignored) (char ')') $ atom `sepBy` separation
    return $ List elements

atom = try bool <|> try stringLiteral <|> try integer <|> try list <|> try quoted <|> try dotted <|> try identifier

identifier = do
    firstChar <- noneOf $ whitespaceChars ++ ['0'..'9'] ++ ['(', ')', ';', '.', '\'']
    rest <- many $ noneOf $ whitespaceChars ++ ['(', ')']
    return $ Var $ firstChar:rest

stringLiteral = do
    char '"'
    chars <- many $ noneOf "\""
    char '"'
    return $ String chars

integer = do
    chars <- many1 $ oneOf ['0'..'9']
    return $ Integer $ read chars

bool = do
    char '#'
    (char 't' >> (return $ Bool True)) <|> (char 'f' >> (return $ Bool False))

quoted = introducedBy '\'' Quoted

dotted = introducedBy '.' Dotted

introducedBy startingChar constructor = do
    char startingChar
    ignored
    expr <- atom
    return $ constructor expr

separation = many1 (whitespace <|> comment)

ignored = many (whitespace <|> comment)

whitespace = do
    c <- oneOf whitespaceChars
    -- Put it in a list so separation and ignored typecheck
    return [c]

comment = do
    char ';'
    many $ noneOf ['\n']

whitespaceChars = ['\n', '\t', '\r', ' ']

-- This is given as (<*) in Real World Haskell but it doesn't seem to exist in the current version of Parsec
returnLeft left right = do
    leftResult <- left
    right
    return leftResult
