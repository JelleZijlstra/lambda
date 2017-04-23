module Ast where

import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Map

data Expr =
    Var String
    | Abstraction String Expr
    | Application Expr Expr
    | Integer Int

type Env = Map String Value

data Value =
    VInteger Int
    | VClosure String Env Expr
    | VBuiltin (Value -> EvalResult Value)

instance Show Expr where
    show (Var s) = s
    show (Abstraction s e) = "\\" ++ s ++ ". " ++ show e
    show (Application e1 e2) = show e1 ++ " " ++ show e2
    show (Integer n) = show n

instance Show Value where
    show (VInteger n) = show n
    show (VClosure s _ e) = "\\" ++ s ++ ". " ++ show e
    show (VBuiltin _) = "(built-in function)"

data EvalResult a =
    Success a (IO ())
    | Failure String (IO ())

instance Functor EvalResult where
    fmap = liftM

instance Applicative EvalResult where
    pure = return
    (<*>) = ap

instance Monad EvalResult where
    (Success x io) >>= f = case f x of
        Success x' io' -> Success x' (io >> io')
        Failure s io' -> Failure s (io >> io')
    (Failure s io) >>= _ = Failure s io
    return x = Success x $ return ()
    fail s = Failure s $ return ()
