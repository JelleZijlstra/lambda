val eval : Ast.expr -> Ast.expr Ast.VarMap.t -> Ast.expr

val eval_expr : Ast.expr -> Ast.environment -> Ast.expr * Ast.environment

val set_args : Ast.expr list -> Ast.expr list -> Ast.environment -> Ast.environment

val add_to_env : string -> Ast.expr -> Ast.environment -> Ast.environment
