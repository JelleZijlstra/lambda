module VarMap = Map.Make(struct type t = string let compare = compare end)

type expr =
	Var of string
	| Integer of int
	| Bool of bool
	| String of string
	| Quoted of expr
	| Dotted of expr
	| List of expr list
	| StatementList of expr list
	| Closure of expr list * environment * expr
	| LibraryFunction of library_function
	| LibraryMacro of library_macro
and environment = expr Environment.env
and library_function = expr list -> expr
and library_macro = expr list -> environment -> expr

let rec join glue lst = match lst with
	| [] -> ""
	| [hd] -> hd
	| hd::tl -> hd ^ glue ^ join glue tl

let rec string_of_expr e =
	match e with
	| Var x -> x
	| Integer i -> string_of_int i
	| Bool true -> "#t"
	| Bool false -> "#f"
	| String s -> "\"" ^ s ^ "\""
	| Quoted e' -> "'" ^ string_of_expr e'
	| Dotted e' -> "." ^ string_of_expr e'
	| StatementList l ->
		let entries = List.map string_of_expr l in
		join " " entries
	| List l ->
		let entries = List.map string_of_expr l in
		"(" ^ join " " entries ^ ")"
	| Closure(_, _, _) -> "<closure>"
	| LibraryFunction _ -> "<built-in function>"
	| LibraryMacro _ -> "<built-in macro>"
