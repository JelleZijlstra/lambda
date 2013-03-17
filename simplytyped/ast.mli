type binop =
	Plus
	| Minus
	| Times

type boolbinop =
	Equals
	| Less
	| Greater

type unop =
	Print

type ltype =
	| Int
	| Bool
	| Function of ltype * ltype

type expr =
	Var of string
	| Abstraction of string * ltype * expr
	| Application of expr * expr
	| Integer of int
	| Boolean of bool
	| Binop of binop * expr * expr
	| Boolbinop of boolbinop * expr * expr
	| If of expr * expr * expr
	| Unop of unop * expr
	| Fix of expr

val string_of_type : ltype -> string

val f_of_binop : binop -> int -> int -> int

val string_of_bool_binop : boolbinop -> string

val f_of_bool_binop : boolbinop -> int -> int -> bool

val string_of_binop : binop -> string

val f_of_unop : unop -> int -> int

val string_of_unop : unop -> string

val string_of_expr : expr -> string
