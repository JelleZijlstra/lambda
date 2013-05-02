type binop =
	Plus
	| Minus
	| Times
	| Divide
	| Modulo

type boolbinop =
	Equals
	| Less
	| Greater

type unop =
	Print

module VarMap : Map.S with type key = string

type ltype =
	| TInt
	| TBool
	| TUnit
	| TFunction of ltype * ltype
	| Typevar of string
	| TypeWithLabel of string * (string * ltype) list
	| TProduct of ltype * ltype
	| TSum of ltype * ltype
	| TRef of ltype
	| TRecord of ltype VarMap.t
	| TForAll of string list * ltype
	| TADT of adt
	| TParameterized of ltype * ltype
	| TModule of module_type_entry list
and adt = adt_cons list
and adt_cons = string * ltype list
and module_type_entry =
	| AbstractType of string * string list
	| ConcreteType of string * string list * ltype
	| Value of string * ltype

type expr =
	Var of string
	| Abstraction of string * ltype option * expr
	| Application of expr * expr
	| In of in_expr * expr
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
	| Record of expr VarMap.t
	| Member of expr * string
	| ConstructorMember of expr * string
	| Unit
	| Constructor of string
	| Match of expr * (pattern * expr) list
	| Error of string
	| Dummy of value
	| Module of ltype option * in_expr list
and pattern =
	PAnything
	| PVariable of string
	| PConstructor of string
	| PApplication of pattern * pattern
	| PInt of int
	| PBool of bool
	| PPair of pattern * pattern
	| PGuarded of pattern * expr
and value =
	| VInt of int
	| VBool of bool
	| VUnit
	| VAbstraction of string * value VarMap.t * expr
	| VReference of value ref
	| VRecord of value VarMap.t
	| VConstructor of string
	| VADTInstance of value * value
	| VPair of value * value
	| VInjection of bool * value
	| VError of string
	| VDummy of expr * value VarMap.t
	| VModule of module_type_entry list * value VarMap.t
and in_expr =
	| Let of string * ltype option * expr
	| LetRec of string * ltype option * expr
	| LetADT of string * string list * adt
	| TypeSynonym of string * ltype
	| SingleExpression of expr
	| Open of string
	| Import of string

type kind =
	| KStar
	| KArrow of kind * kind
	| KVar of string

val string_of_type : ltype -> string

val string_of_module_type_entry : module_type_entry -> string

val f_of_binop : binop -> int -> int -> int

val string_of_bool_binop : boolbinop -> string

val f_of_bool_binop : boolbinop -> int -> int -> bool

val string_of_binop : binop -> string

val f_of_unop : unop -> int -> int

val string_of_unop : unop -> string

val string_of_expr : expr -> string

val string_of_in_expr : in_expr -> string

val string_of_value : value -> string

val string_of_kind: kind -> string

val new_typevar : unit -> ltype

val new_kindvar : unit -> kind

val next_id : unit -> string

val qualify_name : string -> string