module VarMap : Map.S with type key = string = Map.Make(struct
	type t = string
	let compare = compare
end)

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

let join glue =
	List.fold_left (fun a e ->
		let start = if a = "" then "" else a ^ glue in
		start ^ e) ""

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
	| VFunc(args, expr) -> "func(" ^ join ", " args ^ ") { return " ^ string_of_expr expr ^ " }"
	| VObject m ->
		let lst = VarMap.fold (fun k v rest -> ("\"" ^ k ^ "\": " ^ string_of_value v)::rest) m [] in
		"{" ^ join ", " lst ^ "}"
and string_of_expr (e : expr) : string =
	match e with
	| Var x -> x
	| Value v -> string_of_value v
	| Let(x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
	| Call(f, args) -> string_of_expr f ^ "(" ^ join ", " (List.map string_of_expr args) ^ ")"
	| Access(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
	| Assignment(e1, e2, e3) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "] = " ^ string_of_expr e3
	| Delete(e1, e2) -> "delete " ^ string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
