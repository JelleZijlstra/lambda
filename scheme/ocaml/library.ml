open Ast

exception RuntimeError of string

let lambda : library_macro = fun args env -> match args with
    | List params::tl -> Closure(params, env, StatementList tl), env
    | _ -> raise (RuntimeError "lambda: invalid arguments")

let define : library_macro = fun args env -> match args with
    | Var name::tl ->
        if VarMap.mem name env.names then raise (RuntimeError "define: cannot redefine variable");
        let value, _ = Eval.eval_expr (StatementList tl) env in
        let env' = Eval.add_to_env name value env in
        value, env'
    | _ -> raise (RuntimeError "define: invalid arguments")

let set : library_macro = fun args env -> match args with
    | Var name::tl ->
        let value, _ = Eval.eval_expr (StatementList tl) env in
        let env' = Eval.add_to_env name value env in
        value, env'
    | _ -> raise (RuntimeError "set: invalid arguments")

let letm : library_macro = fun args env -> match args with
    | List definitions::expr ->
        let foldf env' definition = match definition with
            | List(Var name::tl) ->
                let value, _ = Eval.eval_expr (StatementList tl) env in
                Eval.add_to_env name value env'
            | _ -> raise (RuntimeError "let: invalid arguments")
        in
        let env' = List.fold_left foldf env definitions in
        let result, _ = Eval.eval_expr (StatementList expr) env' in
        result, env
    | _ -> raise (RuntimeError "let: invalid arguments")

let ifm : library_macro = fun args env -> match args with
    | [condition_expr; iftrue; iffalse] ->
        let condition, _ = Eval.eval_expr condition_expr env in
        (match condition with
            | Bool false -> Eval.eval_expr iffalse env
            | _ -> Eval.eval_expr iftrue env)
    | _ -> raise (RuntimeError "if: invalid arguments")

let eval : library_macro = fun args env -> match args with
    | [expr] ->
        let argument, _ = Eval.eval_expr expr env in
        Eval.eval_expr argument env
    | _ -> raise (RuntimeError "if: invalid arguments")

let defmacro : library_macro = fun args define_env -> match args with
    | Var name::List params::tl ->
        let new_macro : library_macro = fun args call_env ->
            let define_env' = Eval.set_args params args define_env in
            let define_env'' = {define_env' with meval_context = Some call_env} in
            Eval.eval_expr (StatementList tl) define_env''
        in
        let env = Eval.add_to_env name (LibraryMacro new_macro) define_env in
        LibraryMacro new_macro, env
    | _ -> raise (RuntimeError "defmacro: invalid arguments")

let meval : library_macro = fun args env -> match args, env with
    | [expr], {meval_context = Some env'} ->
        let argument, _ = Eval.eval_expr expr env in
        Eval.eval_expr argument env'
    | _ -> raise (RuntimeError "if: invalid arguments")

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

let library = List.fold_left (fun m (name, value) -> VarMap.add name value m) VarMap.empty functions;;
