open Ast

module StringSet = Set.Make(struct
	type t = string
	let compare = compare
end)

let set_of_list lst = List.fold_left (fun x y -> StringSet.add y x) StringSet.empty lst

let next_var : unit -> string =
	let curr = ref 0 in
	fun () ->
		let res = "var" ^ (string_of_int (!curr)) in
		curr := !curr + 1;
		res

(* A reaction flask that we can add and remove molecules to *)
module type FLASK = sig
	type 'a t
	val empty : 'a t
	val is_empty : 'a t -> bool
	val put : 'a -> 'a t -> 'a t
	val get : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
	val to_string : ('a -> string) -> 'a t -> string
end

module StackFlask : FLASK = struct
	type 'a t = 'a list
	let empty = []
	(* is_empty = (=) [] fails. Odd restriction in OCaml? *)
	let is_empty s = s = []
	let put m s = m::s
	let get s = match s with
		| [] -> None
		| hd::tl -> Some(hd, tl)
	let fold f u s = List.fold_right f s u
	let to_string f s = fold (fun m rest -> (f m) ^ "\n" ^ rest) "" s
end

module QueueFlask : FLASK = struct
	type 'a t = 'a list
	let empty = []
	let is_empty s = s = []
	let put m q = q @ [m]
	let get s = match s with
		| [] -> None
		| hd::tl -> Some(hd, tl)
	let fold f u s = List.fold_right f s u
	let to_string f s = fold (fun m rest -> (f m) ^ "\n" ^ rest) "" s
end

module RandomFlask : FLASK = struct
	type 'a t = int * (int * 'a) list
	let empty = 0, []
	let is_empty (_, s) = s = []
	let put m (n, l) = (n + 1, (n, m)::l)
	let get ((n, s) : 'a t) : ('a * 'a t) option = match s with
		| [] -> None
		| _::_ ->
			let len = List.length s in
			let index = Random.int len in
			let (i, m) = List.nth s index in
			let s' = List.filter (fun (n, _) -> n <> i) s in
			Some(m, (n, s'))
	let fold f u (_, s) = List.fold_right (fun (_, m) r -> f m r) s u
	let to_string f s = fold (fun m rest -> (f m) ^ "\n" ^ rest) "" s
end

module Flask : FLASK = RandomFlask

type 'a flask = 'a Flask.t

let get_two (f : 'a flask) : ('a * 'a * 'a flask) option =
	match Flask.get f with
	| None -> None
	| Some(m1, f') -> (match Flask.get f' with
		| None -> None
		| Some(m2, f'') -> Some(m1, m2, f''))

let rec rv (jp : join_pattern) : StringSet.t =
	match jp with
	| JReceive(_, msg) -> set_of_list msg
	| JConjunction(jp1, jp2) -> StringSet.union (rv jp1) (rv jp2)

let rec dvj (jp : join_pattern) : StringSet.t =
	match jp with
	| JReceive(c, _) -> StringSet.singleton c
	| JConjunction(jp1, jp2) -> StringSet.union (dvj jp1) (dvj jp2)

let rec dvd (d : definition) : StringSet.t =
	match d with
	| DDefined(jp, _) -> dvj jp
	| DConjunction(d1, d2) -> StringSet.union (dvd d1) (dvd d2)

let rec fvd (d : definition) : StringSet.t =
	match d with
	| DDefined(jp, p) -> StringSet.union (dvj jp) (StringSet.diff (fvp p) (rv jp))
	| DConjunction(d1, d2) -> StringSet.union (fvd d1) (fvd d2)
and fvp (p : process) : StringSet.t =
	match p with
	| PSend(c, msg) -> StringSet.add c (set_of_list msg)
	| PCompose(p1, p2) -> StringSet.union (fvp p1) (fvp p2)
	| PDef(d, p) -> StringSet.diff (StringSet.union (fvd d) (fvp p)) (dvd d)
	| PNull | PPrint _ -> StringSet.empty

(* Substitutions - haven't yet figured out the alpha-renaming things, so just
	rename everything. *)
let rec substitute_message (f : string) (t : string) (msg : message) : message =
	match msg with
	| [] -> []
	| hd::tl when hd = f -> t::(substitute_message f t tl)
	| hd::tl -> hd::(substitute_message f t tl)

let rec substitute_jp (f : string) (t : string) (jp : join_pattern) : join_pattern =
	match jp with
	| JReceive(c, msg) when c = f -> JReceive(t, substitute_message f t msg)
	| JReceive(c, msg) -> JReceive(c, substitute_message f t msg)
	| JConjunction(jp1, jp2) -> JConjunction(substitute_jp f t jp1, substitute_jp f t jp2)

let rec substitute_d (f : string) (t : string) (d : definition) : definition =
	match d with
	| DDefined(jp, p) -> DDefined(substitute_jp f t jp, substitute_p f t p)
	| DConjunction(d1, d2) -> DConjunction(substitute_d f t d1, substitute_d f t d2)
and substitute_p (f : string) (t : string) (p : process) : process =
	match p with
	| PSend(c, msg) when c = f -> PSend(t, substitute_message f t msg)
	| PSend(c, msg) -> PSend(c, substitute_message f t msg)
	| PCompose(p1, p2) -> PCompose(substitute_p f t p1, substitute_p f t p2)
	| PDef(d, p) -> PDef(substitute_d f t d, substitute_p f t p)
	| PNull -> PNull
	| PPrint n -> PPrint n

let subst_list_d (frm_lst : string list) (to_lst : string list) (d : definition) : definition =
	List.fold_left2 (fun d f t -> substitute_d f t d) d frm_lst to_lst

let subst_list_p (frm_lst : string list) (to_lst : string list) (p : process) : process =
	List.fold_left2 (fun p f t -> substitute_p f t p) p frm_lst to_lst

type env = definition flask * process flask

type reaction = env -> env option

let str_null : reaction = fun (ds, ps) -> Some (ds, Flask.put PNull ps)
let str_null_r : reaction = fun (ds, ps) -> match Flask.get ps with
	| Some(PNull, ps') -> Some(ds, ps')
	| _ -> None

let str_join : reaction = fun (ds, ps) -> match Flask.get ps with
	| Some(PCompose(p1, p2), ps') -> Some(ds, Flask.put p1 (Flask.put p2 ps'))
	| _ -> None
let str_join_r : reaction = fun (ds, ps) -> match get_two ps with
	| Some(p1, p2, ps') -> Some(ds, Flask.put (PCompose(p1, p2)) ps')
	| None -> None

let str_and : reaction = fun (ds, ps) -> match Flask.get ds with
	| Some(DConjunction(d1, d2), ds') -> Some(Flask.put d1 (Flask.put d2 ds'), ps)
	| _ -> None
let str_and_r : reaction = fun (ds, ps) -> match get_two ds with
	| Some(d1, d2, ds') -> Some(Flask.put (DConjunction(d1, d2)) ds', ps)
	| None -> None

let str_def : reaction = fun (ds, ps) -> match Flask.get ps with
	| Some(PDef(d, p), ps') ->
		let dvs = StringSet.fold (fun k v -> k::v) (dvd d) [] in
		let replace = List.map (fun _ -> next_var()) dvs in
		let d' = subst_list_d dvs replace d in
		let p' = subst_list_p dvs replace p in
		Some(Flask.put d' ds, Flask.put p' ps')
	| _ -> None
let str_def_r : reaction = fun (ds, ps) -> match Flask.get ds, Flask.get ps with
	| Some(d, ds'), Some(p, ps') -> Some(ds', Flask.put (PDef(d, p)) ps')
	| _ -> None

let print : reaction = fun (ds, ps) -> match Flask.get ps with
	| Some(PPrint n, ps') ->
		Printf.printf "%d\n" n;
		Some(ds, ps')
	| _ -> None

let react : reaction = fun (ds, ps) -> match Flask.get ds with
	| Some(DDefined(jp, p), _) ->
		let rec find_molecules (jp : join_pattern) (ps : process flask) =
			match jp with
			| JReceive(c, msg) -> (match Flask.get ps with
				| Some(PSend(c', msg'), ps') when c = c' ->
					Some([(msg, msg')], ps')
				| _ -> None)
			| JConjunction(jp1, jp2) -> (match find_molecules jp1 ps with
				| Some(lst1, ps') -> (match find_molecules jp2 ps' with
					| Some(lst2, ps'') -> Some(lst1 @ lst2, ps'')
					| _ -> None)
				| _ -> None)
		in
		(match find_molecules jp ps with
		| None -> None
		| Some(lst, ps') ->
			let p' = List.fold_left (fun p (f, t) -> subst_list_p f t p) p lst in
			Some(ds, Flask.put p' ps'))
	| _ -> None

let reactions : reaction list = [str_null_r; str_join; str_join_r; str_def; str_def_r; str_and; str_and_r; print; react]

(* Choose a random element *)
let choose (lst : 'a list) : 'a =
	let len = List.length lst in
	List.nth lst (Random.int len)

let rec eval_rec ((ds, ps) as env) : unit =
	if Flask.is_empty ps then () else
	let reaction = choose reactions in
	match reaction env with
	| Some(ds', ps') ->
		Printf.printf "---\n";
		Printf.printf "Definitions:\n%s" (Flask.to_string string_of_definition ds');
		Printf.printf "Processes:\n%s" (Flask.to_string string_of_process ps');
		eval_rec (ds', ps')
	| None -> eval_rec (ds, ps)

let eval x = eval_rec (Flask.empty, Flask.put x Flask.empty)

