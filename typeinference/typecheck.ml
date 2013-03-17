open Ast

type lconstraint = Equals of ltype * ltype

type errmsg = string

module TypingContext = Map.Make(struct
	type t = string
	let compare = compare
end)

module ConstraintSet = Set.Make(struct
	type t = lconstraint
	let compare = compare
end)

let em = ConstraintSet.empty

type context = ltype TypingContext.t

type type_or_error = Type of ltype * ConstraintSet.t | Error of errmsg

let rec get_type (e : expr) (c : context) : type_or_error =
	match e with
	| Integer _ -> Type(Int, em)
	| Boolean _ -> Type(Bool, em)
	| Unit -> Type(Uni, em)
	| Var x ->
		(try Type(TypingContext.find x c, em) with Not_found -> Error("Unbound variable: " ^ x))
	| Binop(_, e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Error e, _ -> Error e
		| _, Error e -> Error e
		| Type(t1, cs1), Type(t2, cs2) ->
			Type(Int, ConstraintSet.add (Equals(t1, Int))
				(ConstraintSet.add (Equals(t2, Int))
					(ConstraintSet.union cs1 cs2))))
	| Boolbinop(_, e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Error e, _ -> Error e
		| _, Error e -> Error e
		| Type(t1, cs1), Type(t2, cs2) ->
			Type(Bool, ConstraintSet.add (Equals(t1, Int))
				(ConstraintSet.add (Equals(t2, Int))
					(ConstraintSet.union cs1 cs2))))
	| Unop(_, e) -> (match get_type e c with
		| Error e -> Error e
		| Type(t, cs) ->
			Type(Int, ConstraintSet.add (Equals(t, Int)) cs))
	| Application(e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Type(t1, cs1), Type(t2, cs2) ->
			let new_type = Ast.new_typevar() in
			let new_cs = ConstraintSet.add (Equals(t1, Function(t2, new_type))) (ConstraintSet.union cs1 cs2) in
			Type(new_type, new_cs)
		| Error e, _ -> Error e
		| _, Error e -> Error e)
	| Abstraction(arg, t, body) -> (match get_type body (TypingContext.add arg t c) with
		| Error e -> Error e
		| Type(t', cs) -> Type(Function(t, t'), cs))
	| Fix e -> (match get_type e c with
		| Error e -> Error e
		| Type(t, cs) ->
			let new_type = Ast.new_typevar() in
			let new_cs = ConstraintSet.add (Equals(t, Function(new_type, new_type))) cs in
			Type(new_type, new_cs))
	| If(e1, e2, e3) -> (match get_type e1 c, get_type e2 c, get_type e3 c with
		| Type(t1, cs1), Type(t2, cs2), Type(t3, cs3) ->
			let new_cs = ConstraintSet.union cs1 (ConstraintSet.union cs2 cs3) in
			let new_cs = ConstraintSet.add (Equals(t1, Bool)) (ConstraintSet.add (Equals(t2, t3)) new_cs) in
			Type(t2, new_cs)
		| Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
	| Pair(e1, e2) -> (match get_type e1 c, get_type e2 c with
		| Type(t1, cs1), Type(t2, cs2) -> Type(Product(t1, t2), ConstraintSet.union cs1 cs2)
		| Error e, _ | _, Error e -> Error e)
	| Projection(b, e) -> (match get_type e c with
		| Error e -> Error e
		| Type(t, cs) ->
			let left_typevar = Ast.new_typevar() in
			let right_typevar = Ast.new_typevar() in
			let new_constraint = Equals(t, Product(left_typevar, right_typevar)) in
			let my_type = if b then right_typevar else left_typevar in
			Type(my_type, ConstraintSet.add new_constraint cs))

exception ImpossibleConstraint of string

type substitution = ltype TypingContext.t

let set_map f s = ConstraintSet.fold (fun elt s -> ConstraintSet.add (f elt) s) s ConstraintSet.empty

let rec replace_in_type typevar new_type t =
	match t with
	| Typevar t' when t' = typevar -> new_type
	| Typevar _ | Int | Bool | Uni -> t
	| Function(t1, t2) -> Function(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| Product(t1, t2) -> Product(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)

let replace_type typevar new_type =
	set_map (fun (Equals(t1, t2)) ->
		Equals(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2))

let rec is_free_variable (t : string) (ty : ltype) = match ty with
	| Typevar t' -> t = t'
	| Int | Bool | Uni -> false
	| Product(t1, t2)
	| Function(t1, t2) -> is_free_variable t t1 || is_free_variable t t2

let rec unify (cs : ConstraintSet.t) : substitution =
	try (let chosen = ConstraintSet.choose cs in
		let new_set = ConstraintSet.remove chosen cs in
		match chosen with
		| Equals(t1, t2) when t1 = t2 -> unify new_set
		| Equals(t', Typevar t)
		| Equals(Typevar t, t') when (not(is_free_variable t t')) ->
			let new_cs = replace_type t t' new_set in
			let rest = unify new_cs in
			TypingContext.add t t' rest
		| Equals(Product(t0, t1), Product(t0', t1'))
		| Equals(Function(t0, t1), Function(t0', t1')) ->
			let new_cs = ConstraintSet.add (Equals(t0, t0')) new_set in
			let new_cs = ConstraintSet.add (Equals(t1, t1')) new_cs in
			unify new_cs
		| Equals(t1, t2) ->
			let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
			raise(ImpossibleConstraint("Cannot unify types: " ^ types)))
	with Not_found -> TypingContext.empty

let typecheck e =
	match get_type e TypingContext.empty with
	| Type(t, cs) -> (try let _ = unify cs in None with ImpossibleConstraint e -> Some e)
	| Error e -> Some e
