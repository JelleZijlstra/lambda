open Ast

type errmsg = string

module TypingContext = Map.Make(struct
	type t = string
	let compare = compare
end)

type context = ltype TypingContext.t

type type_or_error = Type of ltype | Error of errmsg

let rec get_type (e : expr) (c : context) : type_or_error =
	match e with
	| Integer _ -> Type Int
	| Boolean _ -> Type Bool
	| Var x ->
		(try Type(TypingContext.find x c) with Not_found -> Error("Unbound variable: " ^ x))
	| Binop(_, e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Type Int, Type Int -> Type Int
		| Error e, _ -> Error e
		| _, Error e -> Error e
		| Type t1, Type t2 ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			Error("Invalid operands to binary expression (expected int and int): " ^ types))
	| Boolbinop(_, e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Type Int, Type Int -> Type Bool
		| Error e, _ -> Error e
		| _, Error e -> Error e
		| Type t1, Type t2 ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			Error("Invalid operands to binary expression (expected int and int): " ^ types))
	| Unop(_, e) -> (match get_type e c with
		| Type Int -> Type Int
		| Error e -> Error e
		| Type t -> Error("Invalid operand to unary expression (expected int): " ^ string_of_type t))
	| Application(e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Type(Function(t1, t2)), Type t3 ->
			if t1 = t3 then Type t2
			else Error("This function expects an argument of type "
				^ string_of_type t1 ^ ", but an argument was supplied of type "
				^ string_of_type t3)
		| Error e, _ -> Error e
		| _, Error e -> Error e
		| Type t, _ -> Error("This expression is of type " ^ string_of_type t ^ " and cannot be applied"))
	| Abstraction(arg, t, body) -> (match get_type body (TypingContext.add arg t c) with
		| Error e -> Error e
		| Type t' -> Type(Function(t, t')))
	| Fix e -> (match get_type e c with
		| Error e -> Error e
		| Type(Function(t1, t2)) when (t1 = t2) -> Type t1
		| Type t -> Error("Fix expects an expression of type t -> t, but this expression has type "
			^ string_of_type t))
	| If(e1, e2, e3) -> (match get_type e1 c, get_type e2 c, get_type e3 c with
		| Type Bool, Type t2, Type t3 when t2 = t3 -> Type t2
		| Type Bool, Type t2, Type t3 -> Error("Branches of an if statement must have the same type: "
			^ string_of_type t2 ^ " and " ^ string_of_type t3)
		| Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
		| Type t, _, _ -> Error("Condition in an if statement must be of type bool, but this expression has type "
			^ string_of_type t))

let typecheck e =
	match get_type e TypingContext.empty with
	| Type _ -> None
	| Error e -> Some e
