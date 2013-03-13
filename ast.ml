type binop =
	Plus
	| Times

type unop =
	Print

type expr =
	Var of string
	| Abstraction of string * expr
	| Application of expr * expr
	| Integer of int
	| Binop of binop * expr * expr
	| Unop of unop * expr

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
	| Abstraction(x, e1) -> "\\" ^ x ^ ". " ^ string_of_expr e1
	| Application(e1, (Application(_, _) as e2)) -> string_of_expr e1 ^ " (" ^ string_of_expr e2 ^ ")"
	| Application(Abstraction(_, _) as e1, e2) -> "(" ^ string_of_expr e1 ^ ") " ^ string_of_expr e2
	| Application(e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
	| Binop(op, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
	| Unop(op, e) -> string_of_unop op ^ " " ^ string_of_expr e
