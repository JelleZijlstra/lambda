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

type expr =
	Var of string
	| Abstraction of string * ltype * expr
	| Application of expr * expr
	| Let of string * ltype * expr * expr
	| LetRec of string * ltype * expr * expr
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

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

let rec string_of_type t = match t with
	| TInt -> "int"
	| TBool -> "bool"
	| TUnit -> "unit"
	| TRef t -> "ref " ^ string_of_type t
	| TFunction(TFunction(_, _) as f, t) ->
		"(" ^ string_of_type f ^ ") -> " ^ string_of_type t
	| TFunction(a, b) -> string_of_type a ^ " -> " ^ string_of_type b
	| Typevar t -> "'" ^ t
	| TProduct(a, b) -> "(" ^ string_of_type a ^ " * " ^ string_of_type b ^ ")"
	| TSum(a, b) -> "(" ^ string_of_type a ^ " | " ^ string_of_type b ^ ")"
	| TRecord(lst) ->
		let foldf accum (l, t) =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " : " ^ string_of_type t in
		"{" ^ List.fold_left foldf "" lst ^ "}"

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
	| Abstraction(x, t, e1) -> "\\" ^ x ^ " : " ^ string_of_type t ^ ". " ^ string_of_expr e1
	| Application(e1, (Application(_, _) as e2)) -> string_of_expr e1 ^ " (" ^ string_of_expr e2 ^ ")"
	| Application(Abstraction(_, _, _) as e1, e2) -> "(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2
	| Application(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
	| Let(x, t, e1, e2) -> "let " ^ x ^ " : " ^ string_of_type t ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
	| LetRec(x, t, e1, e2) -> "let rec " ^ x ^ " : " ^ string_of_type t ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
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
	| Reference _ -> "<loc>"
	| Record lst ->
		let foldf accum (l, e) =
			let start = if accum = "" then "" else accum ^ ", " in
			start ^ l ^ " = " ^ string_of_expr e in
		"{" ^ List.fold_left foldf "" lst ^ "}"
	| Member(e, l) -> string_of_expr e ^ "." ^ l

let new_typevar =
	let current = ref 0 in
	fun () ->
		let n = !current in
		current := n + 1;
		Typevar("typevar" ^ string_of_int n)
;;
