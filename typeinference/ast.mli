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
	| TInt
	| TBool
	| TUnit
	| TFunction of ltype * ltype
	| Typevar of string
	| TProduct of ltype * ltype
	| TSum of ltype * ltype
	| TRef of ltype
	| TRecord of (string * ltype) list
	| TForAll of string list * ltype
	| TADT of adt
	| TParameterized of ltype * ltype
and adt = adt_cons list
and adt_cons = string * ltype list

type expr =
	Var of string
	| Abstraction of string * ltype * expr
	| Application of expr * expr
	| Let of string * ltype * expr * expr
	| LetRec of string * ltype * expr * expr
	| LetType of string * string list * adt * expr
	| Int of int
	| Bool of bool
	| Binop of binop * expr * expr
	| Boolbinop of boolbinop * expr * expr
	| If of expr * expr * expr
	| Unop of unop * expr
	| Fix of expr
	| Pair of expr * expr
	| Projection of bool * expr
	| Case of expr * expr * expr
	| Injection of bool * expr
	| Sequence of expr * expr
	| Assignment of expr * expr
	| Dereference of expr
	| Allocation of expr
	| Reference of expr ref
	| Record of (string * expr) list
	| Member of expr * string
	| Unit
	| Constructor of string
	| ADTInstance of expr * expr
	| Match of expr * (pattern * expr) list
and pattern =
	PAnything
	| PVariable of string
	| PConstructor of string
	| PApplication of pattern * pattern
	| PInt of int
	| PBool of bool
	| PPair of pattern * pattern

type value =
	| VInt of int
	| VBool of bool
	| VAbstraction of string * ltype * expr
	| VReference of value ref
	| VRecord of (string * value) list
	| VUnit
	| VConstructor of string * value
	| VPair of value * value
	| VInjection of bool * value

val string_of_type : ltype -> string

val f_of_binop : binop -> int -> int -> int

val string_of_bool_binop : boolbinop -> string

val f_of_bool_binop : boolbinop -> int -> int -> bool

val string_of_binop : binop -> string

val f_of_unop : unop -> int -> int

val string_of_unop : unop -> string

val string_of_expr : expr -> string

val string_of_value : value -> string

val new_typevar : unit -> ltype
