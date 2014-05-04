module Main where

import qualified System.Environment as Env
import Control.Exception(catch)
import System.IO.Error(IOError)

import Ast
import Eval
import Library
import Parser

schemeRoot :: IO String
schemeRoot = Env.getEnv "SCHEME_ROOT" `catch` handler
    where
        handler :: IOError -> IO String
        handler _ = return "../"

libraryFile :: IO String
libraryFile = do
    root <- schemeRoot
    return $ root ++ "/common/library.scm"

executeFile :: Environment -> String -> IO ()
executeFile env file = do
    text <- readFile file
    case parseScheme text file of
        Left err -> putStrLn $ show err
        Right expr -> do
            eval expr env
            return ()


main = do
    [file] <- Env.getArgs
    env <- library
    libraryFileName <- libraryFile
    executeFile env libraryFileName
    executeFile env file
