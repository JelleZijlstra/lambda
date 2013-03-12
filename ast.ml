type binop =
	Plus
	| Times

type expr =
	Var of string
	| Abstraction of string * expr
	| Application of expr * expr
	| Integer of int
	| Binop of binop * expr * expr

let f_of_binop op = match op with
	| Plus -> (+)
	| Times -> ( * )

let stringify_binop b = match b with
	| Plus -> "+"
	| Times -> "*"

let rec stringify e =
	match e with
	| Var x -> x
	| Integer i -> string_of_int i
	| Abstraction(x, e1) -> "\\" ^ x ^ ". " ^ stringify e1
	| Application(e1, (Application(_, _) as e2)) -> stringify e1 ^ " (" ^ stringify e2 ^ ")"
	| Application(Abstraction(_, _) as e1, e2) -> "(" ^ stringify e1 ^ ") " ^ stringify e2
	| Application(e1, e2) -> stringify e1 ^ " " ^ stringify e2
	| Binop(op, e1, e2) -> stringify e1 ^ " " ^ stringify_binop op ^ " " ^ stringify e2
