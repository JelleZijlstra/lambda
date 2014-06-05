open Ast

let echo_f v =
    let str = Ast.string_of_value v in
    Printf.printf "%s" str;
    VUnit

let print_f v =
    let str = Ast.string_of_value v in
    Printf.printf "%s\n" str;
    VUnit

let add_strings_f s1 =
    Ast.VBuiltin (fun s2 -> match s1, s2 with
        | VString s1', VString s2' -> VString(s1' ^ s2')
        | _ -> failwith("Typecheck failure"))

let get_builtins () =
    let m = VarMap.empty in
    (* echo : forall a. a -> () *)
    let m = VarMap.add "echo" (TForAll(["a"], TFunction(TNamedType "a", TUnit)), echo_f) m in
    (* print : forall a. a -> () *)
    let m = VarMap.add "print" (TForAll(["a"], TFunction(TNamedType "a", TUnit)), print_f) m in
    (* add_strings : string -> string -> string *)
    let m = VarMap.add "add_strings" (TFunction(TString, TFunction(TString, TString)), add_strings_f) m in
    m

let builtins = get_builtins()

let builtin_types = VarMap.map (fun (t, _) -> t) builtins

let builtin_values = VarMap.map (fun (_, v) -> v) builtins
