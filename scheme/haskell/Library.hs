module Library(library) where

import Ast
import Eval

library :: IO Environment
library = do
    env <- makeEnv Nothing Nothing
    mapM_ (uncurry $ setEnv env) functions
    return env

functions :: [(String, Expr)]
functions = [
    ("lambda", LibraryMacro lambda),
    ("define", LibraryMacro define),
    ("set!", LibraryMacro set),
    ("let", LibraryMacro letm),
    ("if", LibraryMacro ifm),
    ("eval", LibraryMacro evalm),
    ("defmacro", LibraryMacro defmacro),
    ("meval", LibraryMacro meval),
    ("print", LibraryFunction printFn),
    ("car", LibraryFunction car),
    ("cdr", LibraryFunction cdr),
    ("cons", LibraryFunction cons),
    ("+", LibraryFunction plus),
    ("append", LibraryFunction append),
    ("null?", LibraryFunction nullFn),
    ("<", LibraryFunction lt),
    (">", LibraryFunction gt),
    ("=", LibraryFunction equals)]

lambda :: LibraryMacro
lambda (List params:tl) env = return $ Closure params env (StatementList tl)
lambda _ _ = fail "lambda: invalid arguments"

define :: LibraryMacro
define (Var name:tl) env = do
    current <- envHas env name
    if current
        then fail "define: cannot redefine variable"
        else do
            value <- eval (StatementList tl) env
            setEnv env name value
            return value
define _ _ = fail "define: invalid arguments"

set :: LibraryMacro
set (Var name:tl) env = do
    value <- eval (StatementList tl) env
    setEnv env name value
    return value
set _ _ = fail "set!: invalid arguments"

letm :: LibraryMacro
letm (List definitions:expr) env = do
    newEnv <- makeEnv (Just env) Nothing
    mapM_ (defineOne newEnv) definitions
    eval (StatementList expr) newEnv
        where
            defineOne newEnv (List (Var name:tl)) = do
                value <- eval (StatementList tl) env
                setEnv newEnv name value
            defineOne _ _ = fail "let: invalid arguments"
letm _ _ = fail "let: invalid arguments"

ifm :: LibraryMacro
ifm [conditionExpr, ifTrue, ifFalse] env = do
    condition <- eval conditionExpr env
    case condition of
        Bool False -> eval ifFalse env
        _ -> eval ifTrue env
ifm _ _ = fail "if: invalid arguments"

evalm :: LibraryMacro
evalm [expr] env = do
    toEval <- eval expr env
    eval toEval env
evalm _ _ = fail "eval: invalid arguments"

defmacro :: LibraryMacro
defmacro (Var name:List params:tl) definitionEnv = do
    setEnv definitionEnv name (LibraryMacro newMacro)
    return $ LibraryMacro newMacro
        where
            newMacro :: LibraryMacro
            newMacro args callEnv = do
                newEnv <- makeEnv (Just definitionEnv) (Just callEnv)
                setArgs params args newEnv
                eval (StatementList tl) newEnv

meval :: LibraryMacro
meval [expr] env = do
    case mevalEnv env of
        Nothing -> fail "meval: invalid arguments"
        Just env' -> do
            toEval <- eval expr env
            eval toEval env'
meval _ _ = fail "meval: invalid arguments"

printFn :: LibraryFunction
printFn args = do
    mapM_ (\arg -> putStrLn $ show arg) args
    return $ List []

car :: LibraryFunction
car [List(hd:_)] = return hd
car _ = fail "car: invalid arguments"

cdr :: LibraryFunction
cdr [List(_:tl)] = return $ List tl
cdr _ = fail "cdr: invalid arguments"

cons :: LibraryFunction
cons [hd, List tl] = return $ List(hd:tl)
cons _ = fail "cons: invalid arguments"

plus :: LibraryFunction
plus args = do
    result <- plus' args
    return $ Integer result
        where
            plus' (Integer hd:tl) = do
                rest <- plus' tl
                return $ hd + rest
            plus' [] = return 0
            plus' _ = fail "+: invalid arguments"

append :: LibraryFunction
append args = do
    result <- append' args
    return $ List $ concat result
        where
            append' (List hd:tl) = do
                rest <- append' tl
                return $ hd:rest
            append' [] = return []
            append' _ = fail "append: invalid arguments"

nullFn :: LibraryFunction
nullFn [List []] = return $ Bool True
nullFn [List _] = return $ Bool False
nullFn _ = fail "null?: invalid arguments"

gt :: LibraryFunction
gt = integerFn ">" (>)

lt :: LibraryFunction
lt = integerFn "<" (<)

integerFn :: String -> (Int -> Int -> Bool) -> LibraryFunction
integerFn name fn [Integer lhs, Integer rhs] = return $ Bool $ fn lhs rhs
integerFn name _ _ = fail $ name ++ ": invalid arguments"

equals :: LibraryFunction
equals [lhs, rhs] = return $ Bool $ lhs == rhs
equals _ = fail "=: invalid arguments"
