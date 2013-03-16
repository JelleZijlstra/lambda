type binop =
	Plus
	| Times

type unop =
	Print

type ltype =
	| Int
	| Function of ltype * ltype

type expr =
	Var of string
	| Abstraction of string * ltype * expr
	| Application of expr * expr
	| Integer of int
	| Binop of binop * expr * expr
	| Unop of unop * expr
	| Fix of expr

val string_of_type : ltype -> string

val f_of_binop : binop -> int -> int -> int

val string_of_binop : binop -> string

val f_of_unop : unop -> int -> int

val string_of_unop : unop -> string

val string_of_expr : expr -> string
