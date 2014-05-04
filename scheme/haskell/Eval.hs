module Eval(eval, setArgs) where

import Ast

eval :: Expr -> Environment -> IO Expr
eval (Integer i) _ = return $ Integer i
eval (String s) _ = return $ String s
eval (Bool b) _ = return $ Bool b
eval fn@(Closure _ _ _) _ = return fn
eval fn@(LibraryFunction _) _ = return fn
eval fn@(LibraryMacro _) _ = return fn

eval (Var x) env = do
    value <- getEnv env x
    case value of
        Nothing -> fail $ "Unbound variable: " ++ x
        Just val -> return val

eval (Quoted e) _ = return e

eval (Dotted _) _ = fail "Cannot evaluate dotted expression"

eval (StatementList []) _ = fail "Empty program"
eval (StatementList [hd]) env = eval hd env
eval (StatementList (hd:tl)) env = do
    eval hd env
    eval (StatementList tl) env

eval (List []) _ = fail "Cannot evaluate empty list"
eval (List (hd:tl)) env = do
    fn <- eval hd env
    case fn of
        LibraryFunction lf -> do
            args <- mapM (flip eval env) tl
            lf args
        LibraryMacro lm -> lm tl env
        Closure params savedEnv body -> do
            args <- mapM (flip eval env) tl
            newEnv <- makeEnv (Just savedEnv) Nothing
            setArgs params args newEnv
            eval body newEnv

setArgs :: [Expr] -> [Expr] -> Environment -> IO ()
setArgs (Var name:params) (value:args) env = do
    setEnv env name value
    setArgs params args env

setArgs [Dotted (Var name)] args env = setEnv env name (List args)
setArgs [] [] _ = return ()

setArgs (Var name:_) [] _ = fail $ "Argument " ++ name ++ " not given"
setArgs _ _ _ = fail "Unable to set arguments"

