open Ast

exception RuntimeError of string

let lambda : library_macro = fun args env -> match args with
    | List params::tl -> Closure(params, env, StatementList tl)
    | _ -> raise (RuntimeError "lambda: invalid arguments")

let define : library_macro = fun args env -> match args with
    | Var name::tl ->
        if Environment.mem env name then raise (RuntimeError "define: cannot redefine variable");
        let value = Eval.eval (StatementList tl) env in
        Environment.set env name value;
        value
    | _ -> raise (RuntimeError "define: invalid arguments")

let set : library_macro = fun args env -> match args with
    | Var name::tl ->
        let value = Eval.eval (StatementList tl) env in
        Environment.set env name value;
        value
    | _ -> raise (RuntimeError "set!: invalid arguments")

let letm : library_macro = fun args env -> match args with
    | List definitions::expr ->
        let new_env = Environment.new_with_parent env in
        let iterf definition = match definition with
            | List(Var name::tl) ->
                let value = Eval.eval (StatementList tl) env in
                Environment.set new_env name value
            | _ -> raise (RuntimeError "let: invalid arguments")
        in
        List.iter iterf definitions;
        Eval.eval (StatementList expr) new_env
    | _ -> raise (RuntimeError "let: invalid arguments")

let ifm : library_macro = fun args env -> match args with
    | [condition_expr; iftrue; iffalse] ->
        (match Eval.eval condition_expr env with
            | Bool false -> Eval.eval iffalse env
            | _ -> Eval.eval iftrue env)
    | _ -> raise (RuntimeError "if: invalid arguments")

let eval : library_macro = fun args env -> match args with
    | [expr] -> Eval.eval (Eval.eval expr env) env
    | _ -> raise (RuntimeError "eval: invalid arguments")

let defmacro : library_macro = fun args define_env -> match args with
    | Var name::List params::tl ->
        let new_macro : library_macro = fun args call_env ->
            let new_env = Environment.new_with_parent_and_meval define_env call_env in
            Eval.set_args params args new_env;
            Eval.eval (StatementList tl) new_env
        in
        Environment.set define_env name (LibraryMacro new_macro);
        LibraryMacro new_macro
    | _ -> raise (RuntimeError "defmacro: invalid arguments")

let meval : library_macro = fun args env -> match args, Environment.get_meval env with
    | [expr], Some env' -> Eval.eval (Eval.eval expr env) env'
    | _ -> raise (RuntimeError "meval: invalid arguments")

let print : library_function = fun args ->
    List.iter (fun arg -> Printf.printf "%s" (Ast.string_of_expr arg ^ "\n")) args;
    List []

let car : library_function = fun args -> match args with
    | [List(hd::_)] -> hd
    | _ -> raise (RuntimeError "car: invalid arguments")

let cdr : library_function = fun args -> match args with
    | [List(_::tl)] -> List tl
    | _ -> raise (RuntimeError "cdr: invalid arguments")

let cons : library_function = fun args -> match args with
    | [hd; List(tl)] -> List(hd::tl)
    | _ -> raise (RuntimeError "cons: invalid arguments")

let plus : library_function = fun args ->
    let numbers = List.map (fun arg -> match arg with
        | Integer n -> n
        | _ -> raise (RuntimeError "+: invalid arugments")) args in
    Integer (List.fold_left (+) 0 numbers)

let append : library_function = fun args ->
    let lists = List.map (fun arg -> match arg with
        | List l -> l
        | _ -> raise (RuntimeError "append: invalid arguments")) args in
    List (List.concat lists)

let null : library_function = fun args -> match args with
    | [List []] -> Bool true
    | [List _] -> Bool false
    | _ -> raise (RuntimeError "null?: invalid arguments")

let gt : library_function = fun args -> match args with
    | [Integer lhs; Integer rhs] -> Bool(lhs > rhs)
    | _ -> raise (RuntimeError ">: invalid arguments")

let lt : library_function = fun args -> match args with
    | [Integer lhs; Integer rhs] -> Bool(lhs < rhs)
    | _ -> raise (RuntimeError "<: invalid arguments")

let equals : library_function = fun args -> match args with
    | [Integer lhs; Integer rhs] -> Bool(lhs = rhs)
    | [Bool lhs; Bool rhs] -> Bool(lhs = rhs)
    | [String lhs; String rhs] -> Bool(lhs = rhs)
    | [List lhs; List rhs] ->
        (try
            Bool (List.for_all2 (=) lhs rhs)
        with Invalid_argument _ -> Bool false)
    | [_; _] -> Bool false
    | _ -> raise (RuntimeError "=: invalid arguments")

let functions = [
    "lambda", LibraryMacro lambda;
    "define", LibraryMacro define;
    "set!", LibraryMacro set;
    "let", LibraryMacro letm;
    "if", LibraryMacro ifm;
    "eval", LibraryMacro eval;
    "defmacro", LibraryMacro defmacro;
    "meval", LibraryMacro meval;
    "print", LibraryFunction print;
    "car", LibraryFunction car;
    "cdr", LibraryFunction cdr;
    "cons", LibraryFunction cons;
    "+", LibraryFunction plus;
    "append", LibraryFunction append;
    "null?", LibraryFunction null;
    "<", LibraryFunction lt;
    ">", LibraryFunction gt;
    "=", LibraryFunction equals;
]

let library =
    let env = Environment.new_env () in
    List.iter (fun (name, value) -> Environment.set env name value) functions;
    env
