module VarMap : Map.S with type key = string = Map.Make(struct
	type t = string
	let compare = compare
end)

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
	| Assignment of expr * expr * expr
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

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

let string_of_binop (b : binop) : string =
	match b with
	| Add -> "+"
	| Subtract -> "-"
	| Multiply -> "*"
	| Divide -> "/"
	| Equals -> "=="
	| NEquals -> "!="
	| LE -> "<="
	| GE -> ">="
	| Less -> "<"
	| Greater -> ">"
	| Concat -> "++"

let string_of_constant (c : constant) : string =
	match c with
	| CInt n -> string_of_int n
	| CString s -> "\"" ^ s ^ "\""
	| CBool true -> "true"
	| CBool false -> "false"
	| CNull -> "null"
	| CUndefined -> "undefined"

let rec string_of_value (v : value) : string =
	match v with
	| VConstant c -> string_of_constant c
	| VClosure(args, _, expr) -> "func(" ^ join ", " args ^ ") { return " ^ string_of_expr expr ^ " }"
	| VObject m ->
		let lst = VarMap.fold (fun k v rest -> ("\"" ^ k ^ "\": " ^ string_of_value v)::rest) m [] in
		"{" ^ join ", " lst ^ "}"
	| VRef v -> "ref " ^ string_of_value (!v)
and string_of_expr (e : expr) : string =
	match e with
	| Var x -> x
	| Value v -> string_of_value v
	| Let(x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
	| Call(f, args) -> string_of_expr f ^ "(" ^ join ", " (List.map string_of_expr args) ^ ")"
	| Access(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
	| Assignment(e1, e2, e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "] = " ^ string_of_expr e3
	| Delete(e1, e2) -> "delete " ^ string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
	| Ref e -> "ref " ^ string_of_expr e
	| Deref e -> "deref " ^ string_of_expr e
	| SetRef(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
	| If(e1, e2, e3) -> "if(" ^ string_of_expr e1 ^ ") { " ^ string_of_expr e2 ^ " } else { " ^ string_of_expr e3 ^ " }"
	| Sequence(e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
	| While(e1, e2) -> "while(" ^ string_of_expr e1 ^ ") { " ^ string_of_expr e2 ^ " }"
	| LabeledBlock(l, e) -> l ^ ": { " ^ string_of_expr e ^ " }"
	| Break(l, e) -> "break " ^ l ^ " " ^ string_of_expr e
	| TryCatch(e1, x, e2) -> "try { " ^ string_of_expr e1 ^ " } catch(" ^ x ^ ") { " ^ string_of_expr e2 ^ " }"
	| TryFinally(e1, e2) -> "try { " ^ string_of_expr e1 ^ " } finally { " ^ string_of_expr e2 ^ " }"
	| Err v -> "err " ^ string_of_value v
	| Binop(b, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop b ^ " " ^ string_of_expr e2
	| Log e -> "log(" ^ string_of_expr e ^ ")"
	| Func(args, expr) -> "func(" ^ join ", " args ^ ") { return " ^ string_of_expr expr ^ " }"
	| Object m ->
		let lst = VarMap.fold (fun k v rest -> ("\"" ^ k ^ "\": " ^ string_of_expr v)::rest) m [] in
		"{" ^ join ", " lst ^ "}"

