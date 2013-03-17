(** Evaluates an expression according to call-by-value semantics *)
val eval_cbv : Ast.expr -> Ast.expr

(** Evaluates an expression according to call-by-name semantics *)
val eval_cbn : Ast.expr -> Ast.expr
