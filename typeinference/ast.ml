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

module VarMap = Map.Make(struct
	type t = string
	let compare = compare
end)
type ltype =
	TInt
	| TBool
	| TUnit
	| TString
	| TFunction of ltype * ltype
	| TNamedType of string
	| TNewType of string
	| Typevar of string * optional_type
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
	| ConcreteType of string * ltype
	| Value of string * ltype
and optional_type = ltype option ref

type expr =
	Var of string
	| Abstraction of string * optional_type * typed_expr
	| Application of typed_expr * typed_expr
	| In of in_expr * typed_expr
	| Int of int
	| Bool of bool
	| String of string
	| Binop of binop * typed_expr * typed_expr
	| Boolbinop of boolbinop * typed_expr * typed_expr
	| If of typed_expr * typed_expr * typed_expr
	| Fix of typed_expr
	| Pair of typed_expr * typed_expr
	| Projection of bool * typed_expr
	| RevealType of typed_expr
	| Case of typed_expr * typed_expr * typed_expr
	| Injection of bool * typed_expr
	| Sequence of typed_expr * typed_expr
	| Assignment of typed_expr * typed_expr
	| Dereference of typed_expr
	| Allocation of typed_expr
	| Record of typed_expr VarMap.t
	| Member of typed_expr * string
	| ConstructorMember of typed_expr * string
	| Unit
	| Constructor of string
	| Match of typed_expr * (pattern * typed_expr) list
	| Error of string
	| Wrapped of typed_expr
	| Dummy of value
	| Module of ltype option * in_expr list
and typed_expr = expr * ltype option ref
and pattern =
	PAnything
	| PVariable of string
	| PConstructor of string
	| PApplication of pattern * pattern
	| PInt of int
	| PBool of bool
	| PString of string
	| PPair of pattern * pattern
	| PGuarded of pattern * typed_expr
	| PAs of pattern * string
and value =
	| VInt of int
	| VBool of bool
	| VString of string
	| VUnit
	| VAbstraction of string * value VarMap.t * typed_expr
	| VReference of value ref
	| VRecord of value VarMap.t
	| VConstructor of string
	| VADTInstance of value * value
	| VPair of value * value
	| VInjection of bool * value
	| VError of string
	| VDummy of typed_expr * value VarMap.t
	| VModule of module_type_entry list * value VarMap.t
	| VBuiltin of builtin_function
and in_expr =
	| Let of string * optional_type * typed_expr
	| LetRec of string * optional_type * typed_expr
	| LetADT of string * string list * adt
	| TypeSynonym of string * ltype
	| SingleExpression of typed_expr
	| Open of string
	| Import of string
and builtin_function = value -> value

type kind =
	| KStar
	| KArrow of kind * kind
	| KVar of string * kind option ref

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

let ljoin glue lst = join "" (List.map (fun e -> glue ^ e) lst)

let quote_string s = "\"" ^ s ^ "\""

let prune_type t = match t with
	| Typevar(n, t_ref) -> (match !t_ref with
		| None -> t
		| Some t' -> t')
	| _ -> t

let rec string_of_type t =
	let t = prune_type t in
	match t with
	| TInt -> "int"
	| TBool -> "bool"
	| TString -> "string"
	| TUnit -> "unit"
	| TRef t -> "ref " ^ string_of_type t
	| TFunction(TFunction(_, _) as f, t) ->
		"(" ^ string_of_type f ^ ") -> " ^ string_of_type t
	| TFunction(a, b) -> string_of_type a ^ " -> " ^ string_of_type b
	| TNamedType n | TNewType n -> n
	| Typevar(t, t_ref) -> "'" ^ t ^ (match !t_ref with
		| None -> ""
		| Some t' -> " (= " ^ string_of_type t' ^ ")")
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
		"{" ^ join ", " (List.map string_of_module_type_entry ts) ^ "}"
and string_of_module_type_entry entry = match entry with
	| AbstractType(n, ps) -> "type " ^ n ^ ljoin " " ps
	| ConcreteType(n, t) -> "type " ^ n ^ " = " ^ string_of_type t
	| Value(n, t) -> n ^ " : " ^ string_of_type t

let f_of_binop op = match op with
	| Plus -> (+)
	| Times -> ( * )
	| Minus -> (-)
	| Modulo -> (mod)
	| Divide -> (/)

let string_of_binop b = match b with
	| Plus -> "+"
	| Times -> "*"
	| Minus -> "-"
	| Modulo -> "%"
	| Divide -> "/"

let string_of_bool_binop b = match b with
	| Equals -> "="
	| Less -> "<"
	| Greater -> ">"

let f_of_bool_binop b = match b with
	| Equals -> (=)
	| Less -> (<)
	| Greater -> (>)

let rec string_of_expr e =
	match e with
	| Var x -> x
	| Unit -> "()"
	| Int i -> string_of_int i
	| String s -> quote_string s
	| Bool true -> "true"
	| Bool false -> "false"
	| Abstraction(x, t, e1) -> (match !t with
		| None -> "\\" ^ x ^ ". " ^ string_of_typed_expr e1
		| Some t' -> "\\" ^ x ^ " : " ^ string_of_type t' ^ ". " ^ string_of_typed_expr e1)
	| Application(e1, ((Application(_, _), _) as e2)) -> string_of_typed_expr e1 ^ " (" ^ string_of_typed_expr e2 ^ ")"
	| Application((Abstraction(_, _, _), _) as e1, e2) -> "(" ^ string_of_typed_expr e1 ^ ") " ^ string_of_typed_expr e2
	| Application(e1, e2) -> string_of_typed_expr e1 ^ " " ^ string_of_typed_expr e2
	| In(e, e2) -> string_of_in_expr e ^ " in " ^ string_of_typed_expr e2
	| Binop(op, e1, e2) -> string_of_typed_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_typed_expr e2
	| Fix e -> "fix " ^ string_of_typed_expr e
	| If(e1, e2, e3) -> "if " ^ string_of_typed_expr e1 ^ " then " ^ string_of_typed_expr e2 ^ " else " ^ string_of_typed_expr e3
	| Boolbinop(op, e1, e2) -> string_of_typed_expr e1 ^ " " ^ string_of_bool_binop op ^ " " ^ string_of_typed_expr e2
	| Pair(e1, e2) -> "(" ^ string_of_typed_expr e1 ^ ", " ^ string_of_typed_expr e2 ^ ")"
	| Projection(false, e) -> "fst " ^ string_of_typed_expr e
	| Projection(true, e) -> "snd " ^ string_of_typed_expr e
	| RevealType(e) -> "reveal_type " ^ string_of_typed_expr e
	| Case(e1, e2, e3) -> "case " ^ string_of_typed_expr e1 ^ " of " ^ string_of_typed_expr e2 ^ " | " ^ string_of_typed_expr e3
	| Injection(false, e) -> "inl " ^ string_of_typed_expr e
	| Injection(true, e) -> "inr " ^ string_of_typed_expr e
	| Sequence(e1, e2) -> string_of_typed_expr e1 ^ "; " ^ string_of_typed_expr e2
	| Assignment(e1, e2) -> string_of_typed_expr e1 ^ " := " ^ string_of_typed_expr e2
	| Allocation e -> "ref " ^ string_of_typed_expr e
	| Dereference e -> "!" ^ string_of_typed_expr e
	| Record lst ->
		let foldf l e accum =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " = " ^ string_of_typed_expr e in
		"{" ^ VarMap.fold foldf lst "" ^ "}"
	| ConstructorMember(e, l)
	| Member(e, l) -> string_of_typed_expr e ^ "." ^ l
	| Constructor n -> n
	| Match(e, lst) ->
		let patterns = List.map (fun (p, e) -> string_of_pattern p ^ " -> " ^ string_of_typed_expr e) lst in
		"match " ^ string_of_typed_expr e ^ " with " ^ join " | " patterns
	| Error s -> "#error " ^ s
	| Wrapped e -> string_of_typed_expr e
	| Dummy v -> string_of_value v
	| Module(t, lst) ->
		let t_str = match t with
			| None -> ""
			| Some t -> string_of_type t ^ "\n\t" in
		let body = join "\n\t" (List.map string_of_in_expr lst) in
		"module\n\t" ^ t_str ^ body ^ "\nend"

and string_of_typed_expr (e, t) = match !t with
	| None -> string_of_expr e
	| Some t' -> "(" ^ string_of_expr e ^ " : " ^ string_of_type t' ^ ")"

and string_of_in_expr e =
	let string_of_let x t e = (match !t with
		| None -> x
		| Some t' -> x ^ " : " ^ string_of_type t') ^ " = " ^ string_of_typed_expr e
	in
	match e with
	| Let(x, t, e1) -> "let " ^ string_of_let x t e1
	| LetRec(x, t, e1) -> "let rec " ^ string_of_let x t e1
	| LetADT(s, params, adt) ->
		let params_str = List.fold_left (^) "" (List.map ((^) " ") params) in
		"type " ^ s ^ params_str ^ " = " ^ string_of_type (TADT adt)
	| TypeSynonym(n, t) -> "type " ^ n ^ " = " ^ string_of_type t
	| SingleExpression e -> string_of_typed_expr e
	| Open m -> "open " ^ m
	| Import m -> "import " ^ m

and string_of_pattern p = match p with
	| PAnything -> "_"
	| PVariable v | PConstructor v -> v
	| PApplication(p1, p2) -> string_of_pattern p1 ^ " " ^ string_of_pattern p2
	| PInt n -> string_of_int n
	| PString s -> quote_string s
	| PBool true -> "true"
	| PBool false -> "false"
	| PPair(p1, p2) -> "(" ^ string_of_pattern p1 ^ ", " ^ string_of_pattern p2 ^ ")"
	| PGuarded(p, e) -> string_of_pattern p ^ " when " ^ string_of_typed_expr e
	| PAs(p, s) -> string_of_pattern p ^ " as " ^ s

and string_of_value e =
	match e with
	| VUnit -> "()"
	| VInt i -> string_of_int i
	| VString s -> quote_string s
	| VBool true -> "true"
	| VBool false -> "false"
	| VAbstraction(x, _, e1) -> "\\" ^ x ^ ". " ^ string_of_typed_expr e1
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
	| VDummy(e, _) -> string_of_typed_expr e
	| VADTInstance(v1, v2) -> "(" ^ string_of_value v1 ^ " " ^ string_of_value v2 ^ ")"
	| VError e -> "#error " ^ e
	| VModule(t, lst) ->
		let foldf k v rest = "\tlet " ^ k ^ " = " ^ string_of_value v ^ "\n" ^ rest in
		"module\n" ^ VarMap.fold foldf lst "" ^ "end"
	| VBuiltin _ -> "<builtin function>"

let rec string_of_kind k = match k with
	| KStar -> "*"
	| KArrow(k1, k2) -> "(" ^ string_of_kind k1 ^ " -> " ^ string_of_kind k2 ^ ")"
	| KVar(k, k_ref) -> "'" ^ k ^ (match !k_ref with
		| None -> ""
		| Some k' -> " (= " ^ string_of_kind k' ^ ")")

let new_typevar =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		Typevar("typevar/" ^ string_of_int n, ref None)
;;

let new_kindvar =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		KVar("kindvar/" ^ string_of_int n, ref None)
;;

let next_id =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		string_of_int n
;;

let qualify_name name = name ^ "/" ^ next_id();;
