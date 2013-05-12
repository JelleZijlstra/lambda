module VarMap : Map.S with type key = string

type constant =
	CInt of int
	| CString of string
	| CBool of bool
	| CNull
	| CUndefined

type value =
	VConstant of constant
	| VFunc of string list * expr
	| VObject of value VarMap.t
and expr =
	Var of string
	| Value of value
	| Let of string * expr * expr
	| Call of expr * expr list
	| Access of expr * expr
	| Assignment of expr * expr * expr
	| Delete of expr * expr

val string_of_expr : expr -> string
val string_of_value : value -> string
