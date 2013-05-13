module VarMap : Map.S with type key = string

type binop =
	Add
	| Multiply
	| Subtract
	| Divide
	| Equals
	| NEquals
	| Less
	| Greater
	| LE
	| GE
	| Concat

type constant =
	CInt of int
	| CString of string
	| CBool of bool
	| CNull
	| CUndefined

type value =
	VConstant of constant
	| VClosure of string list * value VarMap.t * expr
	| VObject of value VarMap.t
	| VRef of value ref
and expr =
	Var of string
	| Value of value
	| Let of string * expr * expr
	| Call of expr * expr list
	| Access of expr * expr
	| Delete of expr * expr
	| Ref of expr
	| Deref of expr
	| SetRef of expr * expr
	| If of expr * expr * expr
	| Sequence of expr * expr
	| While of expr * expr
	| LabeledBlock of string * expr
	| Break of string * expr
	| TryCatch of expr * string * expr
	| TryFinally of expr * expr
	| Err of value
	| Binop of binop * expr * expr
	| Log of expr
	| Func of string list * expr
	| Object of expr VarMap.t

val string_of_expr : expr -> string
val string_of_value : value -> string
val string_of_binop : binop -> string
