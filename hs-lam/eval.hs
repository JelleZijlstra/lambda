module Eval where

import Ast
import Env
import qualified Data.Map as Map

eval' :: Expr -> Env -> EvalResult Value
eval' (Var x) e = case Map.lookup x e of
    Just v -> return v
    Nothing -> fail $ "Unbound variable " ++ x

eval' (Abstraction x b) e = return $ VClosure x e b

eval' (Application e1 e2) e = do
    v1 <- eval' e1 e
    v2 <- eval' e2 e
    case v1 of
        VClosure x e b -> eval' b (Map.insert x v2 e)
        VBuiltin f -> f v2
        _ -> fail "This expression is not a function; it cannot be applied"

eval' (Integer n) _ = return $ VInteger n

eval' (String s) _ = return $ VString s

eval :: Expr -> IO ()
eval e = break_it $ eval' e env
    where
        break_it (Success v io) = do
            io
            putStrLn $ "Result: " ++ show v
        break_it (Failure s io) = do
            io
            putStrLn $ "Error: " ++ s
