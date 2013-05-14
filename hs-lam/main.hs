module Main where

import System.Environment

import Ast
import Lexer
import Parser
import Eval

main :: IO ()
main = do
    (filename:_) <- getArgs
    text <- readFile filename
    eval $ hslam $ alexScanTokens text
    return ()
