type errmsg = string

type typecheck_t = TError of errmsg | Result of Ast.expr

val typecheck : Ast.expr -> bool -> string -> typecheck_t
