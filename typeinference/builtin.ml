open Ast

let echo_f v =
    let str = Ast.string_of_value v in
    Printf.printf "%s" str;
    VUnit

let print_f v =
    let str = Ast.string_of_value v in
    Printf.printf "%s\n" str;
    VUnit

let get_builtins () =
    let m = VarMap.empty in
    (* echo : forall a. a -> () *)
    let m = VarMap.add "echo" (TForAll(["a"], TFunction(Typevar "a", TUnit)), echo_f) m in
    (* print : forall a. a -> () *)
    let m = VarMap.add "print" (TForAll(["a"], TFunction(Typevar "a", TUnit)), print_f) m in
    m

let builtins = get_builtins()

let builtin_types = VarMap.map (fun (t, _) -> t) builtins

let builtin_values = VarMap.map (fun (_, v) -> v) builtins
