module Ast where

import Data.List(intercalate)
import Data.Maybe(isJust)
import qualified Data.HashTable.IO as H

type MutableMap k v = H.BasicHashTable k v

type VarMap = MutableMap String Expr

data Expr =
    Var String
    | Integer Int
    | Bool Bool
    | String String
    | Quoted Expr
    | Dotted Expr
    | List [Expr]
    | StatementList [Expr]
    | Closure [Expr] Environment Expr
    | LibraryFunction LibraryFunction
    | LibraryMacro LibraryMacro

type LibraryFunction = [Expr] -> IO Expr
type LibraryMacro = [Expr] -> Environment -> IO Expr

data Environment = Environment {
    names :: VarMap,
    parent :: Maybe Environment,
    mevalEnv :: Maybe Environment
}

instance Show Expr where
    show (Var x) = x
    show (Integer i) = show i
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (String s) = "\"" ++ s ++ "\""
    show (Quoted e) = "'" ++ show e
    show (Dotted e) = ". " ++ show e
    show (StatementList l) = unwords $ map show l
    show (List l) = "(" ++ unwords (map show l) ++ ")"
    show (Closure {}) = "<closure>"
    show (LibraryFunction _) = "<built-in function>"
    show (LibraryMacro _) = "<built-in macro>"

instance Eq Expr where
    Integer lhs == Integer rhs = lhs == rhs
    Bool lhs == Bool rhs = lhs == rhs
    String lhs == String rhs = lhs == rhs
    List lhs == List rhs = lhs == rhs
    _ == _ = False

makeEnv :: Maybe Environment -> Maybe Environment -> IO Environment
makeEnv parent mevalEnv = do
    names <- H.new
    return $ Environment names parent mevalEnv

setEnv :: Environment -> String -> Expr -> IO ()
setEnv env = H.insert $ names env

getEnv :: Environment -> String -> IO (Maybe Expr)
getEnv env name = do
    current <- H.lookup (names env) name
    case current of
        Just val -> return $ Just val
        Nothing -> case parent env of
            Nothing -> return Nothing
            Just parentEnv -> getEnv parentEnv name

envHas :: Environment -> String -> IO Bool
envHas env name = do
    value <- getEnv env name
    return $ isJust value
