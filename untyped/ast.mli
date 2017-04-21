type binop =
	Plus
	| Times

type unop =
	Print

module VarMap : Map.S with type key = string

type expr =
	Var of string
	| Abstraction of string * expr
	| Application of expr * expr
	| Integer of int
	| String of string
	| Binop of binop * expr * expr
	| Unop of unop * expr

type value =
	VClosure of string * value VarMap.t * expr
	| VInteger of int
	| VString of string
	| VDummy of value VarMap.t * expr

val f_of_binop : binop -> int -> int -> int

val string_of_binop : binop -> string

val string_of_value : value -> string

val f_of_unop : unop -> int -> int

val string_of_unop : unop -> string

val string_of_expr : expr -> string
