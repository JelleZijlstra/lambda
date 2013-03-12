type expr =
	Var of string
	| Abstraction of string * expr
	| Application of expr * expr

let rec stringify e =
	match e with
	| Var x -> x
	| Abstraction(x, e1) -> "\\" ^ x ^ ". " ^ stringify e1
	| Application(Application(_, _) as e1, e2)
	| Application(Abstraction(_, _) as e1, e2) -> "(" ^ stringify e1 ^ ") " ^ stringify e2
	| Application(e1, e2) -> stringify e1 ^ " " ^ stringify e2
