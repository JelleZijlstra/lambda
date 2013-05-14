module Ast where

import Data.Map

data Expr =
    Var String
    | Abstraction String Expr
    | Application Expr Expr
    | Integer Int
    | Binop Binop Expr Expr
    | Print Expr

type Env = Map String Value

data Value =
    VInteger Int
    | VClosure String Env Expr

data Binop =
    Plus | Times

instance Show Expr where
    show (Var s) = s
    show (Abstraction s e) = "\\" ++ s ++ ". " ++ show e
    show (Application e1 e2) = show e1 ++ " " ++ show e2
    show (Integer n) = show n
    show (Binop b e1 e2) = show e1 ++ " " ++ show b ++ " " ++ show e2
    show (Print e) = "print " ++ show e

instance Show Binop where
    show Plus = "+"
    show Times = "*"

instance Show Value where
    show (VInteger n) = show n
    show (VClosure s _ e) = "\\" ++ s ++ ". " ++ show e
