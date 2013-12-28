val eval : Ast.expr -> Ast.expr Environment.env -> Ast.expr

val set_args : Ast.expr list -> Ast.expr list -> Ast.expr Environment.env -> unit
