open Ast

type errmsg = string

type typecheck_t = TError of errmsg | Result of Ast.typed_expr

type lconstraint = Equals of ltype * ltype | HasLabel of ltype * string * ltype

type kconstraint = KEquals of kind * kind

module ConstraintSet = Set.Make(struct
	type t = lconstraint
	let compare = compare
end)

module KindConstraintSet = Set.Make(struct
	type t = kconstraint
	let compare = compare
end)

module VariableSet = Set.Make(struct
	type t = string
	let compare = compare
end)

module Context = struct
	(* variable -> type, type -> kind, type synonym -> underlying type, location *)
	type t = ltype VarMap.t * kind VarMap.t * ltype VarMap.t * string

	let empty loc = VarMap.empty, VarMap.empty, VarMap.empty, loc

	let add_var v t (ts, ks, tss, l) =
		VarMap.add v t ts, ks, tss, l

	let find_var v (ts, _, _, _) = VarMap.find v ts

	let fold_vars f u (ts, _, _, _) = VarMap.fold f ts u

	let add_kind t k (ts, ks, vss, l) =
		ts, VarMap.add t k ks, vss, l

	let find_kind t (_, ks, _, _) = VarMap.find t ks

	let add_type_synonym x t (ts, ks, vss, l) =
		ts, ks, VarMap.add x t vss, l

	let find_type_synonym x (_, _, vss, _) = VarMap.find x vss

	let get_loc (_, _, _, l) = l

	let set_loc l (ts, ks, vss, _) = (ts, ks, vss, l)
end

let initial_context loc =
	let builtins = Builtin.builtin_types in
	let c = Context.empty loc in
	VarMap.fold (fun k t rest -> Context.add_var k t rest) builtins c

let em = ConstraintSet.empty
let kem = KindConstraintSet.empty

exception TypeError of string

type type_cs = Type of ltype * expr * ConstraintSet.t * KindConstraintSet.t

let prune_expr (e, t) = match !t with
	| None ->
		let tv = Ast.new_typevar() in
		t := Some tv; tv
	| Some t' ->
		let pruned = prune_type t' in
		t := Some pruned;
		pruned

let rec free_variables (ty : ltype) : VariableSet.t = match ty with
	| Typevar(t', _) -> VariableSet.singleton t'
	| TNamedType t' -> VariableSet.singleton t'
	| TInt | TBool | TUnit | TString | TNewType _ -> VariableSet.empty
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TParameterized(t1, t2)
	| TFunction(t1, t2) -> VariableSet.union (free_variables t1) (free_variables t2)
	| TRef t' -> free_variables t'
	| TypeWithLabel(_, lst) ->
		List.fold_left VariableSet.union VariableSet.empty (List.map (fun (_, t) -> free_variables t) lst)
	| TRecord lst ->
		let types = VarMap.fold (fun _ t rest -> free_variables t::rest) lst [] in
		List.fold_left VariableSet.union VariableSet.empty types
	| TForAll(lst, t') -> List.fold_left (fun x y -> VariableSet.remove y x) (free_variables t') lst
	| TADT(lst) ->
		let mapf (_, t) = List.fold_left VariableSet.union VariableSet.empty (List.map free_variables t) in
		List.fold_left VariableSet.union VariableSet.empty (List.map mapf lst)
	| TModule lst ->
		let foldf entry rest = match entry with
			| AbstractType(n, _) -> VariableSet.remove n rest
			| ConcreteType(n, t) -> VariableSet.remove n (VariableSet.union (free_variables t) rest)
			| Value(n, t) -> VariableSet.union (free_variables t) rest in
		List.fold_right foldf lst VariableSet.empty

let free_variables_context (c : Context.t) =
	Context.fold_vars (fun _ t a -> VariableSet.union (free_variables t) a) VariableSet.empty c

let rec replace_in_type typevar new_type t =
	let t = prune_type t in
	match t with
	| Typevar(t', t_ref) -> (match !t_ref with
		| None -> Typevar(t', t_ref)
		| Some t -> replace_in_type typevar new_type t)
	| TNamedType t' when t' = typevar -> new_type
	| TypeWithLabel(n, lst) when n = typevar -> new_type
	| TNamedType _ | TNewType _ | TInt | TBool | TUnit | TString -> t
	| TFunction(t1, t2) -> TFunction(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TProduct(t1, t2) -> TProduct(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TSum(t1, t2) -> TSum(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TRef t -> TRef(replace_in_type typevar new_type t)
	| TRecord lst -> TRecord(VarMap.map (replace_in_type typevar new_type) lst)
	| TForAll(lst, t') -> if List.mem typevar lst then t else TForAll(lst, replace_in_type typevar new_type t')
	| TADT(lst) ->
		let mapf (l, t) = l, List.map (replace_in_type typevar new_type) t in
		TADT(List.map mapf lst)
	| TParameterized(t1, t2) -> TParameterized(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
	| TypeWithLabel(n, lst) ->
		let mapf (l, t) = l, replace_in_type typevar new_type t in
		TypeWithLabel(n, List.map mapf lst)
	| TModule lst ->
		let rec loop lst = match lst with
			| [] -> []
			| ((AbstractType(n, _)) | (ConcreteType(n, _)))::tl when n = typevar -> lst
			| ConcreteType(n, t)::tl ->
				ConcreteType(n, replace_in_type typevar new_type t)::loop tl
			| AbstractType(n, params)::tl -> AbstractType(n, params)::loop tl
			| Value(n, t)::tl -> Value(n, replace_in_type typevar new_type t)::loop tl in
		TModule(loop lst)

let quantify (ctxt : Context.t) (t : ltype) : ltype =
	let vars = VariableSet.diff (free_variables t) (free_variables_context ctxt) in
	if VariableSet.is_empty vars then t else TForAll(VariableSet.elements vars, t)

let instantiate t = match t with
	| TForAll(lst, t') -> List.fold_left (fun t tv -> replace_in_type tv (new_typevar()) t) t' lst
	| _ -> t

(* Select the more specialized type (useful for module typing at check time) *)
let rec select_type t1 t2 = match t1, t2 with
	| Typevar _, _ -> t2
	| _, Typevar _ -> t1
	| TFunction(t1a, t1b), TFunction(t2a, t2b) -> TFunction(select_type t1a t1b, select_type t2a t2b)
	| TProduct(t1a, t1b), TProduct(t2a, t2b) -> TProduct(select_type t1a t1b, select_type t2a t2b)
	| TSum(t1a, t1b), TSum(t2a, t2b) -> TSum(select_type t1a t1b, select_type t2a t2b)
	| _ -> t1 (* Not worth it for the rest *)

let mapify_module_type (actual : module_type_entry list) : module_type_entry VarMap.t * ltype VarMap.t =
	let foldf (tmap, vmap) entry = match entry with
		| ConcreteType(n, _) | AbstractType(n, _) ->
			(VarMap.add n entry tmap, vmap)
		| Value(x, t) ->
			(tmap, VarMap.add x t vmap)
	in
	List.fold_left foldf (VarMap.empty, VarMap.empty) actual

exception ImpossibleConstraint of string

type substitution = ltype VarMap.t
type ksubstitution = kind VarMap.t

let print_cs cs = ConstraintSet.fold (fun e a -> (match e with
	| Equals(t1, t2) -> "\t" ^ string_of_type t1 ^ " = " ^ string_of_type t2
	| HasLabel(t1, l, t2) -> "\t" ^ string_of_type t1 ^ " has label " ^ l ^ " of type " ^ string_of_type t2) ^ "\n" ^ a) cs ""

let print_ks ks = KindConstraintSet.fold (fun (KEquals(k1, k2)) a ->
	"\t" ^ string_of_kind k1 ^ " = " ^ string_of_kind k2 ^ "\n" ^ a) ks ""

let print_substitution s =
	let foldf x t rest = "\t" ^ x ^ " = " ^ string_of_type t ^ "\n" ^ rest in
	VarMap.fold foldf s ""

let print_ksubstitution s =
	let foldf x t rest = "\t" ^ x ^ " = " ^ string_of_kind t ^ "\n" ^ rest in
	VarMap.fold foldf s ""

let set_map f s = ConstraintSet.fold (fun elt s -> ConstraintSet.add (f elt) s) s em

let replace_type typevar new_type =
	let mapf t = match t with
		| Equals(t1, t2) ->
			Equals(replace_in_type typevar new_type t1, replace_in_type typevar new_type t2)
		| HasLabel(t1, l, t2) ->
			HasLabel(replace_in_type typevar new_type t1, l, replace_in_type typevar new_type t2) in
	set_map mapf

let rec is_free_variable (t : ltype option ref) (ty : ltype) = match ty with
	| Typevar(_, t') -> t == t'
	| TNamedType _ | TNewType _ | TInt | TBool | TUnit | TString -> false
	| TProduct(t1, t2)
	| TSum(t1, t2)
	| TParameterized(t1, t2)
	| TFunction(t1, t2) -> is_free_variable t t1 || is_free_variable t t2
	| TRef t' -> is_free_variable t t'
	| TRecord lst -> VarMap.exists (fun _ t' -> is_free_variable t t') lst
	| TForAll(lst, t') -> is_free_variable t t'
	| TADT(lst) -> List.exists (fun (_, t') -> List.exists (is_free_variable t) t') lst
	| TypeWithLabel(n, lst) -> List.exists (fun (_, t') -> is_free_variable t t') lst
	| TModule lst ->
		let rec f item = match item with
			| Value(n, t') -> is_free_variable t t'
			| ConcreteType(_, t') -> is_free_variable t t'
			| AbstractType(_, _) -> false in
		Util.any f lst

let add_label (x : string) (label : string) (t : ltype) (s : substitution) : substitution =
	try match VarMap.find x s with
		| TypeWithLabel(x', lst) when x = x' ->
			VarMap.add x (TypeWithLabel(x', (label, t)::lst)) s
		| _ -> s
	with Not_found -> VarMap.add x (TypeWithLabel(x, [(label, t)])) s

let rec unify_types t1 t2 c =
	let t1 = prune_type t1 in
	let t2 = prune_type t2 in
	match t1, t2 with
	| _, _ when t1 = t2 -> ()
	| TNamedType tv, t2 ->
		(try
			let t' = Context.find_type_synonym tv c in
			unify_types t' t2 c
		with Not_found -> raise(TypeError("Unbound type alias: " ^ tv)))
	| t1, TNamedType tv ->
		(try
			let t' = Context.find_type_synonym tv c in
			unify_types t1 t' c
		with Not_found -> raise(TypeError("Unbound type alias: " ^ tv)))
	| t', Typevar(t, t_ref) when (not(is_free_variable t_ref t')) ->
		t_ref := Some t'
	| Typevar(t, t_ref), t' when (not(is_free_variable t_ref t')) ->
		t_ref := Some t'
	| TProduct(t0, t1), TProduct(t0', t1')
	| TSum(t0, t1), TSum(t0', t1')
	| TFunction(t0, t1), TFunction(t0', t1')
	| TParameterized(t0, t1), TParameterized(t0', t1') ->
		unify_types t0 t0' c; unify_types t1 t1' c
	| TRef t0, TRef t1 ->
		unify_types t0 t1 c
	| t, (TForAll(_, _) as fa)
	| (TForAll(_, _) as fa), t ->
		unify_types t (instantiate fa) c
	| TRecord lst1, TRecord lst2 when VarMap.cardinal lst1 = VarMap.cardinal lst2 ->
		let foldf l t1 _ =
			let t2 = VarMap.find l lst2 in
			unify_types t1 t2 c in
		VarMap.fold foldf lst1 ()
	| _, _ ->
		let types = string_of_type t1 ^ " and " ^ string_of_type t2 in
		raise(ImpossibleConstraint("Cannot unify types: " ^ types))

let rec prune_kind k =
	match k with
	| KStar -> KStar
	| KArrow(k1, k2) -> KArrow(k1, k2)
	| KVar(k, k_ref) -> (match !k_ref with
		| None -> KVar(k, k_ref)
		| Some k' -> prune_kind k')

let rec unify_kinds k1 k2 =
	let k1, k2 = prune_kind k1, prune_kind k2 in
	match k1, k2 with
	| _, _ when k1 = k2 -> ()
	| KStar, KStar -> ()
	| KArrow(k1', k1''), KArrow(k2', k2'') ->
		unify_kinds k1' k2';
		unify_kinds k1'' k2''
	| KVar(_, k_ref), k | k, KVar(_, k_ref) ->
		k_ref := Some k
	| _ ->
		let kinds = string_of_kind k1 ^ " and " ^ string_of_kind k2 in
		raise(ImpossibleConstraint("Cannot unify kinds: " ^ kinds))

let rec get_kind (t : ltype) (c : Context.t) : ltype * kind =
	let type_binary t1 t2 =
		let t1, k1 = get_kind t1 c in
		unify_kinds k1 KStar;
		let t2, k2 = get_kind t2 c in
		unify_kinds k2 KStar;
		t1, t2 in
	match t with
	| TInt | TBool | TUnit | TString -> t, KStar
	| TProduct(t1, t2) -> let t1, t2 = type_binary t1 t2 in TProduct(t1, t2), KStar
	| TSum(t1, t2) -> let t1, t2 = type_binary t1 t2 in TSum(t1, t2), KStar
	| TFunction(t1, t2) -> let t1, t2 = type_binary t1 t2 in TFunction(t1, t2), KStar
	| TRef t ->
		let t, k = get_kind t c in
		unify_kinds k KStar;
		TRef t, KStar
	| TNamedType tv ->
		(try
			let k = Context.find_kind tv c in
			let t' = Context.find_type_synonym tv c in
			t', k
		with Not_found -> raise(TypeError("Unbound type alias in kind check: " ^ tv)))
	| TNewType name ->
		(try
			let k = Context.find_kind name c in
			TNewType name, k
		with Not_found -> raise(TypeError("Unbound ADT type in kind check: " ^ name)))
	| Typevar(tv, t_ref) -> (match !t_ref with
		| None -> Typevar(tv, t_ref), KStar
		| Some t' ->
			let t', k' = get_kind t' c in
			unify_kinds k' KStar;
			t', KStar)
	| TypeWithLabel(n, lst) -> raise(TypeError("Ignoring TypeWithLabel"))
(* 		let foldf (l, t) (lst, rest) =
			let t, k, kc = get_kind t c in
			(l, t)::lst, add (KEquals(k, KStar)) (union kc rest) in
		let lst, kc = List.fold_right foldf lst ([], kem) in
		TypeWithLabel(n, lst), KStar, kc *)
	| TRecord lst ->
		let foldf l t map =
			let t, k = get_kind t c in
			unify_kinds k KStar;
			VarMap.add l t map in
		let map = VarMap.fold foldf lst VarMap.empty in
		TRecord map, KStar
	| TForAll([], t') -> get_kind t' c
	| TForAll(lst, t') ->
		let foldf (c, vars) x =
			let kv = Ast.new_kindvar() in
			let c' = Context.add_kind x kv c in
			let c' = Context.add_type_synonym x (TNamedType x) c' in
			c', kv::vars in
		let new_c, vars = List.fold_left foldf (c, []) lst in
		let t', res = get_kind t' new_c in
		let k = List.fold_left (fun rest kv -> KArrow(kv, rest)) res vars in
		TForAll(lst, t'), k
	| TADT lst ->
		let foldf (cons, lst) t =
			let inner_foldf t params =
				let t1, k1 = get_kind t c in
				unify_kinds k1 KStar;
				t1::params in
			let params = List.fold_right inner_foldf lst [] in
			(cons, params)::t in
		let lst' = List.fold_right foldf lst [] in
		TADT lst', KStar
	| TParameterized(t1, t2) ->
		let t1', k1 = get_kind t1 c in
		let t2', k2 = get_kind t2 c in
		let result_k = Ast.new_kindvar() in
		unify_kinds k1 (KArrow(k2, result_k));
		TParameterized(t1', t2'), result_k
	| TModule lst ->
 		let rec loop lst c = match lst with
			| [] -> []
			| Value(n, t)::tl ->
				let t', k = get_kind t c in
				unify_kinds k KStar;
				let lst = loop tl c in
				Value(n, t')::lst
			| ConcreteType(n, TForAll(lst, TADT t))::tl ->
				let t', c' = type_adt n lst t c in
				let lst' = loop tl c' in
				ConcreteType(n, TForAll(lst, TADT t'))::lst'
			| ConcreteType(n, TADT t)::tl ->
				let t', c' = type_adt n [] t c in
				let lst' = loop tl c' in
				ConcreteType(n, TADT t')::lst'
			| ConcreteType(_, _)::_ ->
				raise(TypeError("Concrete module type must be ADT for now"))
			| AbstractType(n, lst)::tl ->
				let k = List.fold_left (fun rest _ -> KArrow(KStar, rest)) KStar lst in
				let c' = Context.add_kind n k c in
				let lst' = loop tl c' in
				AbstractType(n, lst)::lst' in
		let lst' = loop lst c in
		TModule lst', KStar

and type_adt (name : string) (params : string list) (lst : adt) (c : Context.t) : adt * Context.t =
	let qualified_name = qualify_name name in
	let result_t = List.fold_left (fun r p -> TParameterized(r, TNamedType p)) (TNewType qualified_name) params in
	let foldf (k, tc) param =
		let kv = Ast.new_kindvar() in
		(* Map type to itself as a hack *)
		let tc = Context.add_type_synonym param (TNamedType param) tc in
		(KArrow(kv, k), Context.add_kind param kv tc) in
	let adt_kind, new_tc = List.fold_left foldf (KStar, c) params in
	let new_tc = Context.add_kind name adt_kind new_tc in
	let new_tc = Context.add_type_synonym name (TNewType qualified_name) new_tc in
	let foldf (c_rest, t_rest) (name, lst) =
		let inner_foldf (rest, new_lst) t =
			let t, k = get_kind t new_tc in
			unify_kinds k KStar;
			TFunction(t, rest), t::new_lst in
		let t, new_lst = List.fold_left inner_foldf (result_t, []) lst in
		let t = if params = [] then t else TForAll(params, t) in
		Context.add_var name t c_rest, (name, new_lst)::t_rest in
	let new_tc, new_lst = List.fold_left foldf (new_tc, []) lst in
	(* Include variables from the ADT definition (constructors) in the new context, but not kind bindings (arguments) *)
	let varc, _, _, _ = new_tc in
	let _, kindc, tss, loc = c in
	let c' = varc, kindc, tss, loc in
	let c' = Context.add_kind name adt_kind c' in
	let c' = Context.add_kind qualified_name adt_kind c' in
	let c' = Context.add_type_synonym name (TNewType qualified_name) c' in
	(new_lst, c')


(* Handle an optional type (that may or may not be given) plus its kind *)
let get_optional_type (t : ltype option ref) (c : Context.t) : ltype =
	let new_t = match !t with
	| None -> Ast.new_typevar()
	| Some t' ->
		let t', k = get_kind t' c in
		(* unify_kinds k KStar; - Breaks for forall variables. *)
		t' in
	let new_t = prune_type new_t in
	t := Some new_t;
	new_t

let check_module_type (interface : module_type_entry list) (actual : module_type_entry list) =
	(* First, create two maps from the actual type, to enable easy lookup
		while looping over the interface. *)
	let tmap, vmap = mapify_module_type actual in

	let check_foldf t entry = try
		match entry with
		| ConcreteType(n, tp) -> (match VarMap.find n tmap with
			| ConcreteType(n', tp') when n = n' && tp = tp' -> entry::t
			| ConcreteType(_, _) as e ->
				let msg = "Interface and implementation do not match: " ^ string_of_module_type_entry entry ^ " and " ^ string_of_module_type_entry e in
				raise(TypeError msg)
			| _ -> failwith "impossible")
		| AbstractType(n, params) -> (match VarMap.find n tmap with
			| ConcreteType(n', _) when n = n' && params = [] -> entry::t
			| ConcreteType(n', TForAll(params', _)) when n = n' && params = params' ->
				entry::t
			| ConcreteType(_, _) as e ->
				let msg = "Interface and implementation do not match: " ^ string_of_module_type_entry entry ^ " and " ^ string_of_module_type_entry e in
				raise(TypeError msg)
			| _ -> failwith "impossible")
		| Value(x, tp) ->
			let tp' = VarMap.find x vmap in
			unify_types tp tp' (Context.empty "module");
			entry::t
	with Not_found ->
		let text = string_of_module_type_entry entry in
		let msg = "Module types do not match: cannot find interface member " ^ text ^ " in implementation" in
		raise(TypeError msg)
	in
	List.fold_left check_foldf [] interface

let rec get_type ((e, given_t) : typed_expr) (c : Context.t) : ltype * typed_expr =
	let inferred_t, e' = get_type_expr e c in
	(match !given_t with
	| None -> ()
	| Some t' -> unify_types t' inferred_t c);
	let inferred_t = prune_type inferred_t in
	given_t := Some inferred_t;
	(inferred_t, (e', given_t))
and get_type_expr (e : expr) (c : Context.t) : ltype * expr =
	match e with
	| Int _ -> TInt, e
	| Bool _ -> TBool, e
	| Unit -> TUnit, e
	| String _ -> TString, e
	| Wrapped e ->
		let t', e' = get_type e c in
		t', Wrapped e'
	| Var x ->
		(try instantiate (Context.find_var x c), e
			with Not_found -> raise (TypeError("Unbound variable: " ^ x)))
	| In(Let(x, t, e1), e2) ->
		let tc, e1' = get_type_let e1 x t c in
		let t2, e2' = get_type e2 tc in
		t2, In(Let(x, t, e1'), e2')
	| In(LetRec(x, t, e1), e2) ->
		let tc, e1' = get_type_let_rec e1 x t c in
		let t2, e2' = get_type e2 tc in
		t2, In(LetRec(x, t, e1'), e2')
	| Binop(op, e1, e2) ->
		let t1, e1 = get_type e1 c in
		unify_types t1 TInt c;
		let t2, e2 = get_type e2 c in
		unify_types t2 TInt c;
		TInt, Binop(op, e1, e2)
	| Boolbinop(op, e1, e2) ->
		let t1, e1 = get_type e1 c in
		unify_types t1 TInt c;
		let t2, e2 = get_type e2 c in
		unify_types t2 TInt c;
		TBool, Boolbinop(op, e1, e2)
	| Application(e1, e2) ->
		let t1, e1 = get_type e1 c in
		let t2, e2 = get_type e2 c in
		let new_type = Ast.new_typevar() in
		unify_types t1 (TFunction(t2, new_type)) c;
		new_type, Application(e1, e2)
	| Abstraction(arg, t, body) ->
		let t' = get_optional_type t c in
		let t'', body = get_type body (Context.add_var arg t' c) in
		TFunction(t', t''), Abstraction(arg, t, body)
	| Fix e ->
		let t, e = get_type e c in
		let new_type = Ast.new_typevar() in
		unify_types t (TFunction(new_type, new_type)) c;
		new_type, Fix e
	| If(e1, e2, e3) ->
		let t1, e1 = get_type e1 c in
		unify_types t1 TBool c;
		let t2, e2 = get_type e2 c in
		let t3, e3 = get_type e3 c in
		unify_types t2 t3 c;
		t2, If(e1, e2, e3)
	| Pair(e1, e2) ->
		let t1, e1 = get_type e1 c in
		let t2, e2 = get_type e2 c in
		TProduct(t1, t2), Pair(e1, e2)
	| Projection(b, e) ->
		let t, e = get_type e c in
		let left_typevar = Ast.new_typevar() in
		let right_typevar = Ast.new_typevar() in
		unify_types t (TProduct(left_typevar, right_typevar)) c;
		let my_type = if b then right_typevar else left_typevar in
		my_type, Projection(b, e)
	| RevealType(e) ->
		let t, e = get_type e c in
		t, RevealType(e)
	| Injection(b, e) ->
		let t, e = get_type e c in
		let other_typevar = Ast.new_typevar() in
		let my_type = if b then TSum(other_typevar, t) else TSum(t, other_typevar) in
		my_type, Injection(b, e)
	| Case(e1, e2, e3) ->
		let t1, e1 = get_type e1 c in
		let t2, e2 = get_type e2 c in
		let t3, e3 = get_type e3 c in
		let tv2a = Ast.new_typevar() in
		let tv3a = Ast.new_typevar() in
		let tv_res = Ast.new_typevar() in
		unify_types t1 (TSum(tv2a, tv3a)) c;
		unify_types t2 (TFunction(tv2a, tv_res)) c;
		unify_types t3 (TFunction(tv3a, tv_res)) c;
		tv_res, Case(e1, e2, e3)
	| Allocation(e) ->
		let t, e = get_type e c in TRef t, Allocation e
	| Dereference(e) ->
		let t, e = get_type e c in
		let tv = Ast.new_typevar() in
		unify_types t (TRef tv) c;
		tv, Dereference e
	| Assignment(e1, e2) ->
		let t1, e1 = get_type e1 c in
		let t2, e2 = get_type e2 c in
		unify_types t1 (TRef t2) c;
		TUnit, Assignment(e1, e2)
	| Sequence(e1, e2) ->
		let t1, e1 = get_type e1 c in
		unify_types t1 TUnit c;
		let t2, e2 = get_type e2 c in
		t2, Sequence(e1, e2)
 	| Record lst ->
		let foldf l e (rest, record_e) =
			let t, e = get_type e c in
			VarMap.add l t rest, VarMap.add l e record_e in
		let t, lst = VarMap.fold foldf lst (VarMap.empty, VarMap.empty) in
		TRecord t, Record lst
	| Member(e, l) ->
		let t, e = get_type e c in
		(match t with
			| TModule lst ->
				(* Only if the e is immediately resolvable as a module do we interpret this as a module access. *)
				(* TODO: Change TModule into a map too. *)
				let rec find_var lst = match lst with
					| [] -> raise(TypeError("Unbound module member " ^ l ^ " in " ^ string_of_type t))
					| Value(n, t')::_ when n = l -> t'
					| ConcreteType(n, TForAll(params, TADT t))::tl ->
						let _, c = type_adt n params t (Context.empty (Context.get_loc c)) in
						(try Context.find_var l c with Not_found -> find_var tl)
					| _::tl -> find_var tl in
				find_var lst, Member(e, l)
			| _ -> failwith "unimplemented"
(* 				let tv = Ast.new_typevar() in
				let new_cs = ConstraintSet.add (HasLabel(t, l, tv)) cs in
				Type(tv, Member(e, l), new_cs, ks) *))
	| ConstructorMember(e, l) ->
		let t, e = get_type e c in
		(match t with
			| TModule lst ->
				let rec find_var lst = match lst with
					| [] -> raise(TypeError("Unbound module member " ^ l ^ " in " ^ string_of_type t))
					| ConcreteType(n, TForAll(params, TADT t))::tl ->
						let _, c = type_adt n params t (Context.empty (Context.get_loc c)) in
						(try Context.find_var l c with Not_found -> find_var tl)
					| _::tl -> find_var tl in
				find_var lst, ConstructorMember(e, l)
			| _ -> raise(TypeError("Constructors can only appear in modules")))
	| Constructor n -> (try instantiate (Context.find_var n c), e
		with Not_found -> raise(TypeError("Unbound constructor " ^ n)))
	| In(LetADT(name, params, lst), e) ->
		let t', c' = type_adt name params lst c in
		let t1, e1 = get_type e c' in
		t1, In(LetADT(name, params, lst), e1)
	| In(TypeSynonym(name, t), e) ->
		let t, kind = get_kind t c in
		let new_tc = Context.add_kind name kind (Context.add_type_synonym name t c) in
		let t1, e1 = get_type e new_tc in
		(* TODO: does this need a constraint t == TNamedType name? *)
		t1, In(TypeSynonym(name, t), e)
	| Dummy _ | Error _ -> raise(TypeError("Should not appear here"))
	| Match(e, lst) ->
		let t1, e1 = get_type e c in
		let res_tv = Ast.new_typevar() in
		let add_list = List.fold_left (fun rest (x, t) -> Context.add_var x t rest) in
		let foldf (p, e) lst =
			let rec type_pattern p t = match p with
				| PAnything -> p, []
				| PVariable x -> p, [(x, t)]
				| PInt _ -> unify_types t TInt c; (p, [])
				| PBool _ -> unify_types t TBool c; (p, [])
				| PString _ -> unify_types t TString c; (p, [])
				| PPair(p1, p2) ->
					let t1 = Ast.new_typevar() in
					let t2 = Ast.new_typevar() in
					let p1, vars1 = type_pattern p1 t1 in
					let p2, vars2 = type_pattern p2 t2 in
					unify_types t (TProduct(t1, t2)) c;
					(PPair(p1, p2), vars1 @ vars2)
				| PConstructor x ->
					let xt = (try instantiate (Context.find_var x c)
						with Not_found -> raise(TypeError("Unbound constructor " ^ x))) in
					unify_types t xt c;
					(p, [])
				| PApplication(p1, p2) ->
					let t1 = Ast.new_typevar() in
					let t2 = Ast.new_typevar() in
					let p1, vars1 = type_pattern p1 t1 in
					let p2, vars2 = type_pattern p2 t2 in
					unify_types t1 (TFunction(t2, t)) c;
					PApplication(p1, p2), vars1 @ vars2
				| PGuarded(p, e) ->
					let p, vars = type_pattern p t in
					let new_tc = add_list c vars in
					let t', e' = get_type e new_tc in
					unify_types t' TBool c;
					(PGuarded(p, e'), vars)
				| PAs(p, x) ->
					let p, vars = type_pattern p t in
					(PAs(p, x), (x, t)::vars)
			in
			let p, vars = type_pattern p t1 in
			let new_tc = add_list c vars in
			let t', e = get_type e new_tc in
			unify_types res_tv t' c;
			(p, e)::lst in
		let lst = List.fold_right foldf lst [] in
		res_tv, Match(e1, lst)
	| Module(t, lst) ->
		let rec loop lst c = match lst with
			| [] -> [], []
			| (TypeSynonym(name, t) as hd)::tl ->
				let t, kind = get_kind t c in
				let new_tc = Context.add_kind name kind (Context.add_type_synonym name t c) in
				let rest, m = loop tl new_tc in
				ConcreteType(name, t)::rest, hd::m
			| (LetADT(name, params, lst) as hd)::tl ->
				let new_ks, c' = type_adt name params lst c in
				let rest, m = loop tl c' in
				ConcreteType(name, TForAll(params, TADT lst))::rest, hd::m
			| Let(x, t, e)::tl ->
				let tc, e = get_type_let e x t c in
				let rest, m = loop tl tc in
				(match !t with
					| None -> failwith "impossible"
					| Some t' -> Value(x, t')::rest, Let(x, t, e)::m)
			| LetRec(x, t, e)::tl ->
				let tc, e = get_type_let_rec e x t c in
				let rest, m = loop tl tc in
				(match !t with
					| None -> failwith "impossible"
					| Some t' -> Value(x, t')::rest, LetRec(x, t, e)::m)
			| SingleExpression e::tl ->
				let _, e = get_type e c in
				let rest, m = loop tl c in
				rest, SingleExpression e::m
			| Open m::tl ->
				let rest, m' = loop tl (get_type_open m c) in
				rest, Open m::m'
			| Import m::tl ->
				let l, tc = get_type_import m c in
				let rest, m' = loop tl tc in
				rest, l::m'
		in
		let lst, m = loop lst c in
		(match t with
			| None -> TModule lst, Module(Some(TModule lst), m)
			| Some(TModule lst') ->
				let _ = check_module_type lst' lst in
				TModule lst', Module(t, m)
			| Some _ -> raise(TypeError("Illegal module type")))
	| In(SingleExpression e1, e2) ->
		let t1, e1 = get_type e1 c in
		let t2, e2 = get_type e2 c in
		t2, Sequence(e1, e2)
	| In(Open m, e) ->
		let c' = get_type_open m c in
		let t, e = get_type e c' in
		t, In(Open m, e)
	| In(Import m, e) ->
		let l, tc = get_type_import m c in
		let t1, e1 = get_type e tc in
		t1, In(l, e1)

and get_type_let (e : typed_expr) (x : string) (t : ltype option ref) (c : Context.t) : Context.t * typed_expr =
	let t1, e1 = get_type e c in
	let t1' = quantify c t1 in
	let t' = get_optional_type t c in
	unify_types t' t1' c;
	let result_t = prune_type t' in
	t := Some result_t;
	let new_tc = Context.add_var x result_t c in
	new_tc, e1
and get_type_let_rec (e : typed_expr) (x : string) (t : ltype option ref) (c : Context.t) : Context.t * typed_expr =
	let t' = get_optional_type t c in
	let e1_c = Context.add_var x t' c in
	let t1, e1 = get_type e e1_c in
	let t1' = quantify e1_c t1 in
	unify_types t' t1' c;
	let result_t = prune_type t' in
	t := Some result_t;
	let new_tc = Context.add_var x result_t c in
	new_tc, e1
and get_type_open x (c : Context.t) : Context.t =
	try match Context.find_var x c with
		| TModule lst ->
			let foldf rest elt = match elt with
				| Value(n, t) ->
					Context.add_var n t rest
				| ConcreteType(n, TForAll(lst, TADT t)) ->
					let kc, c' = type_adt n lst t rest in
					c'
				| ConcreteType(_, _) ->
					raise(TypeError("Concrete module type must be ADT for now"))
				| AbstractType(n, lst) ->
					let k = List.fold_left (fun rest _ -> KArrow(KStar, rest)) KStar lst in
					Context.add_kind n k rest
			in
			List.fold_left foldf c lst
		| _ as t ->
			raise(TypeError("open expression must be statically resolve as a module type; got " ^ string_of_type t))
	with Not_found -> raise(TypeError("Unbound module: " ^ x))
and get_type_import (m : string) (c : Context.t) =
	let com, new_loc = Util.find_module m (Context.get_loc c) in
	let t = ref None in
	let tc, e = get_type_let com m t (initial_context new_loc) in
	Let(m, t, e), tc

let typecheck e verbose loc =
	try
		let t, e' = get_type e (initial_context loc) in
		Result e'
	with
		| ImpossibleConstraint e -> TError e
		| TypeError e -> TError e
