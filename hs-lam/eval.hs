module Eval where

import Ast
import qualified Data.Map as Map

data EvalResult a =
    Success a (IO ())
    | Failure String (IO ())

instance Monad EvalResult where
    (Success x io) >>= f = case f x of
        Success x' io' -> Success x' (io >> io')
        Failure s io' -> Failure s (io >> io')
    (Failure s io) >>= _ = Failure s io
    return x = Success x $ return ()
    fail s = Failure s $ return ()

output :: String -> EvalResult ()
output s = Success () $ putStrLn s

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
        _ -> fail "This expression is not a function; it cannot be applied"

eval' (Integer n) _ = return $ VInteger n

eval' (Binop b e1 e2) e = do
    v1 <- eval' e1 e
    v2 <- eval' e2 e
    case (v1, v2) of
        (VInteger n1, VInteger n2) -> return $ VInteger $ f_of_binop b n1 n2
            where f_of_binop Plus = (+)
                  f_of_binop Times = (*)
        (_, _) -> fail "Binop must be of integers"

eval' (Print e) env = do
    v <- eval' e env
    output $ show v
    return v

eval :: Expr -> IO ()
eval e = break_it $ eval' e Map.empty
    where
        break_it (Success v io) = do
            io
            putStrLn $ "Result: " ++ show v
        break_it (Failure s io) = do
            io
            putStrLn $ "Error: " ++ s
