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

module VarMap = Map.Make(struct
	type t = string
	let compare = compare
end)

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
and in_expr =
	| Let of string * ltype option * expr
	| LetRec of string * ltype option * expr
	| LetADT of string * string list * adt
	| TypeSynonym of string * ltype
	| SingleExpression of expr

type kind =
	| KStar
	| KArrow of kind * kind
	| KVar of string

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

let ljoin glue lst = join "" (List.map (fun e -> glue ^ e) lst)

let rec string_of_type t = match t with
	| TInt -> "int"
	| TBool -> "bool"
	| TUnit -> "unit"
	| TRef t -> "ref " ^ string_of_type t
	| TFunction(TFunction(_, _) as f, t) ->
		"(" ^ string_of_type f ^ ") -> " ^ string_of_type t
	| TFunction(a, b) -> string_of_type a ^ " -> " ^ string_of_type b
	| Typevar t -> "'" ^ t
	| TypeWithLabel(t, lst) -> "(" ^ t ^ " with "
		^ join ", " (List.map (fun (l, t) -> l ^ " : " ^ string_of_type t) lst) ^ ")"
	| TProduct(a, b) -> "(" ^ string_of_type a ^ " * " ^ string_of_type b ^ ")"
	| TSum(a, b) -> "(" ^ string_of_type a ^ " | " ^ string_of_type b ^ ")"
	| TRecord(lst) ->
		let foldf l t accum =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " : " ^ string_of_type t in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| TForAll(lst, t) -> "forall " ^ join ", " lst ^ ". " ^ string_of_type t
	| TADT(lst) -> join " | " (List.map (fun (name, args) -> name ^ " " ^ join " " (List.map string_of_type args)) lst)
	| TParameterized(t1, t2) -> string_of_type t1 ^ " " ^ string_of_type t2
	| TModule(ts) ->
		let mapf entry = match entry with
			| AbstractType(n, ps) -> "type " ^ n ^ ljoin " " ps
			| ConcreteType(n, ps, t) -> "type " ^ n ^ ljoin " " ps ^ " = " ^ string_of_type t
			| Value(n, t) -> n ^ " : " ^ string_of_type t in
		join ", " (List.map mapf ts)

let f_of_binop op = match op with
	| Plus -> (+)
	| Times -> ( * )
	| Minus -> (-)

let string_of_binop b = match b with
	| Plus -> "+"
	| Times -> "*"
	| Minus -> "-"

let string_of_bool_binop b = match b with
	| Equals -> "="
	| Less -> "<"
	| Greater -> ">"

let f_of_bool_binop b = match b with
	| Equals -> (=)
	| Less -> (<)
	| Greater -> (>)

let f_of_unop op = match op with
	| Print -> (fun x -> Printf.printf "%d\n" x; x)

let string_of_unop op = match op with
	| Print -> "print"

let rec string_of_expr e =
	match e with
	| Var x -> x
	| Unit -> "()"
	| Int i -> string_of_int i
	| Bool true -> "true"
	| Bool false -> "false"
	| Abstraction(x, Some t, e1) -> "\\" ^ x ^ " : " ^ string_of_type t ^ ". " ^ string_of_expr e1
	| Abstraction(x, None, e1) -> "\\" ^ x ^ ". " ^ string_of_expr e1
	| Application(e1, (Application(_, _) as e2)) -> string_of_expr e1 ^ " (" ^ string_of_expr e2 ^ ")"
	| Application(Abstraction(_, _, _) as e1, e2) -> "(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2
	| Application(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
	| In(e, e2) -> string_of_in_expr e ^ " in " ^ string_of_expr e2
	| Binop(op, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
	| Unop(op, e) -> string_of_unop op ^ " " ^ string_of_expr e
	| Fix e -> "fix " ^ string_of_expr e
	| If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
	| Boolbinop(op, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_bool_binop op ^ " " ^ string_of_expr e2
	| Pair(e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
	| Projection(false, e) -> "fst " ^ string_of_expr e
	| Projection(true, e) -> "snd " ^ string_of_expr e
	| Case(e1, e2, e3) -> "case " ^ string_of_expr e1 ^ " of " ^ string_of_expr e2 ^ " | " ^ string_of_expr e3
	| Injection(false, e) -> "inl " ^ string_of_expr e
	| Injection(true, e) -> "inr " ^ string_of_expr e
	| Sequence(e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
	| Assignment(e1, e2) -> string_of_expr e1 ^ " := " ^ string_of_expr e2
	| Allocation e -> "ref " ^ string_of_expr e
	| Dereference e -> "!" ^ string_of_expr e
	| Record lst ->
		let foldf l e accum =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " = " ^ string_of_expr e in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| Member(e, l) -> string_of_expr e ^ "." ^ l
	| Constructor n -> n
	| Match(e, lst) ->
		let patterns = List.map (fun (p, e) -> string_of_pattern p ^ " -> " ^ string_of_expr e) lst in
		"match " ^ string_of_expr e ^ " with " ^ join " | " patterns
	| Error s -> "#error " ^ s
	| Dummy v -> string_of_value v
	| Module(t, lst) ->
		let t_str = match t with
			| None -> ""
			| Some t -> string_of_type t ^ "\n\t" in
		let body = join "\n\t" (List.map string_of_in_expr lst) in
		"module\n\t" ^ t_str ^ body ^ "\nend"
and string_of_in_expr e = match e with
	| Let(x, None, e1) -> "let " ^ x ^ " = " ^ string_of_expr e1
	| Let(x, Some t, e1) -> "let " ^ x ^ " : " ^ string_of_type t ^ " = " ^ string_of_expr e1
	| LetRec(x, None, e1) -> "let rec " ^ x ^ " = " ^ string_of_expr e1
	| LetRec(x, Some t, e1) -> "let rec " ^ x ^ " : " ^ string_of_type t ^ " = " ^ string_of_expr e1
	| LetADT(s, params, adt) ->
		let params_str = List.fold_left (^) "" (List.map ((^) " ") params) in
		"type " ^ s ^ params_str ^ " = " ^ string_of_type (TADT adt)
	| TypeSynonym(n, t) -> "type " ^ n ^ " = " ^ string_of_type t
	| SingleExpression e -> string_of_expr e

and string_of_pattern p = match p with
	| PAnything -> "_"
	| PVariable v | PConstructor v -> v
	| PApplication(p1, p2) -> string_of_pattern p1 ^ " " ^ string_of_pattern p2
	| PInt n -> string_of_int n
	| PBool true -> "true"
	| PBool false -> "false"
	| PPair(p1, p2) -> "(" ^ string_of_pattern p1 ^ ", " ^ string_of_pattern p2 ^ ")"
and string_of_value e =
	match e with
	| VUnit -> "()"
	| VInt i -> string_of_int i
	| VBool true -> "true"
	| VBool false -> "false"
	| VAbstraction(x, _, e1) -> "\\" ^ x ^ ". " ^ string_of_expr e1
	| VPair(v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
	| VInjection(false, v) -> "inl " ^ string_of_value e
	| VInjection(true, v) -> "inr " ^ string_of_value e
	| VReference _ -> "<loc>"
	| VRecord lst ->
		let foldf l e accum =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " = " ^ string_of_value e in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| VConstructor c -> c
	| VDummy(e, _) -> string_of_expr e
	| VADTInstance(v1, v2) -> "(" ^ string_of_value v1 ^ " " ^ string_of_value v2 ^ ")"
	| VError e -> "#error " ^ e

let rec string_of_kind k = match k with
	| KStar -> "*"
	| KArrow(k1, k2) -> "(" ^ string_of_kind k1 ^ " -> " ^ string_of_kind k2 ^ ")"
	| KVar x -> x

let new_typevar =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		Typevar("typevar" ^ string_of_int n)
;;

let new_kindvar =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		KVar("kindvar" ^ string_of_int n)
;;

let next_id =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		string_of_int n
;;
