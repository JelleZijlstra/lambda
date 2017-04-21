module Env(env) where

import Ast
import qualified Data.Map as Map

printB :: Value -> EvalResult Value
printB v = do
    Success v $ putStrLn $ show v
    return v

env :: Map.Map String Value
env = Map.fromList [("print", VBuiltin printB)]
