module VarMap : Map.S with type key = string

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

val string_of_expr : expr -> string
