type 'a env

val new_env : unit -> 'a env
val new_with_parent : 'a env -> 'a env
val new_with_parent_and_meval : 'a env -> 'a env -> 'a env
val set : 'a env -> string -> 'a -> unit
val mem : 'a env -> string -> bool
val get : 'a env -> string -> 'a option
val get_meval : 'a env -> 'a env option
