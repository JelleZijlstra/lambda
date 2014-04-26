module Main where

import System.Environment

import Ast
import Eval
import Library
import Parser

main = do
    [file] <- getArgs
    text <- readFile file
    case parseScheme text file of
        Left err -> putStrLn $ show err
        Right expr -> do
            env <- library
            eval expr env
            return ()
