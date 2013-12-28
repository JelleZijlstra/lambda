let initial_size = 16

type 'a env = {
    names : (string, 'a) Hashtbl.t;
    parent : 'a env option;
    meval_env : 'a env option
}

let new_env () = {
    names = Hashtbl.create initial_size;
    parent = None;
    meval_env = None
}

let new_with_parent parent = {
    names = Hashtbl.create initial_size;
    parent = Some parent;
    meval_env = parent.meval_env
}

let new_with_parent_and_meval parent meval_env = {
    names = Hashtbl.create initial_size;
    parent = Some parent;
    meval_env = Some meval_env;
}

let set env key value =
    Hashtbl.replace env.names key value

let rec get env key =
    try Some (Hashtbl.find env.names key)
    with Not_found -> (match env.parent with
        None -> None
        | Some parent -> get parent key)

let mem env = Hashtbl.mem env.names

let get_meval env = env.meval_env
