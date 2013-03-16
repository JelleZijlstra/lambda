type binop =
	Plus
	| Times

type unop =
	Print

type ltype =
	| Int
	| Function of ltype * ltype

type expr =
	Var of string
	| Abstraction of string * ltype * expr
	| Application of expr * expr
	| Integer of int
	| Binop of binop * expr * expr
	| Unop of unop * expr
	| Fix of expr

let rec string_of_type t = match t with
	| Int -> "int"
	| Function(Function(_, _) as f, t) -> "(" ^ string_of_type f ^ ") -> " ^ string_of_type t
	| Function(a, b) -> string_of_type a ^ " -> " ^ string_of_type b

let f_of_binop op = match op with
	| Plus -> (+)
	| Times -> ( * )

let string_of_binop b = match b with
	| Plus -> "+"
	| Times -> "*"

let f_of_unop op = match op with
	| Print -> (fun x -> Printf.printf "%d\n" x; x)

let string_of_unop op = match op with
	| Print -> "print"

let rec string_of_expr e =
	match e with
	| Var x -> x
	| Integer i -> string_of_int i
	| Abstraction(x, t, e1) -> "\\" ^ x ^ " : " ^ string_of_type t ^ ". " ^ string_of_expr e1
	| Application(e1, (Application(_, _) as e2)) -> string_of_expr e1 ^ " (" ^ string_of_expr e2 ^ ")"
	| Application(Abstraction(_, _, _) as e1, e2) -> "(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2
	| Application(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
	| Binop(op, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
	| Unop(op, e) -> string_of_unop op ^ " " ^ string_of_expr e
	| Fix e -> "fix " ^ string_of_expr e
