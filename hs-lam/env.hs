module Env(env) where

import Ast
import qualified Data.Map as Map

printB :: Value -> EvalResult Value
printB v = do
    Success v $ putStrLn $ show v
    return v

binOp :: (Int -> Int -> Int) -> Value -> EvalResult Value
binOp op v = do
    case v of
        VInteger arg1 -> return $ VBuiltin curried
            where
                curried :: Value -> EvalResult Value
                curried (VInteger arg2) = return $ VInteger (arg1 `op` arg2)
                curried _ = fail "binop argument must be an integer"
        _ -> fail "binop argument must be an integer"

env :: Map.Map String Value
env = Map.fromList [("print", VBuiltin printB), ("+", VBuiltin $ binOp (+)), ("*", VBuiltin $ binOp (*))]
