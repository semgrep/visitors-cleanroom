(* Functor-based visitors for the same types as the OO visitors in
   test_types.ml.  These use the recursive-module pattern from
   https://gist.github.com/nmote/cf32653a31a0938d5b757b24e2193f86
   for open recursion instead of OO virtual dispatch. *)

open Types_ours.Test_types

(* ================================================================== *)
(* expr / stmt — iter                                                  *)
(* ================================================================== *)

module ExprIter (Env : sig type env end) = struct
  module type S = sig
    val traverse_expr : Env.env -> expr -> unit
    val traverse_stmt : Env.env -> stmt -> unit
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_expr env = function
      | Lit _ -> ()
      | Add (e1, e2) ->
        Self.traverse_expr env e1;
        Self.traverse_expr env e2
      | Neg e -> Self.traverse_expr env e
      | Let (_, e1, e2) ->
        Self.traverse_expr env e1;
        Self.traverse_expr env e2
      | Var _ -> ()
      | IfZero (c, t, f) ->
        Self.traverse_expr env c;
        Self.traverse_expr env t;
        Self.traverse_expr env f
      | Call (_, args) ->
        List.iter (Self.traverse_expr env) args

    let traverse_stmt env = function
      | Assign (_, e) -> Self.traverse_expr env e
      | Seq stmts -> List.iter (Self.traverse_stmt env) stmts
      | While (e, s) ->
        Self.traverse_expr env e;
        Self.traverse_stmt env s
      | Return e -> Self.traverse_expr env e
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end

(* ================================================================== *)
(* expr / stmt — map                                                   *)
(* ================================================================== *)

module ExprMap (Env : sig type env end) = struct
  module type S = sig
    val traverse_expr : Env.env -> expr -> expr
    val traverse_stmt : Env.env -> stmt -> stmt
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_expr env = function
      | Lit i -> Lit i
      | Add (e1, e2) -> Add (Self.traverse_expr env e1, Self.traverse_expr env e2)
      | Neg e -> Neg (Self.traverse_expr env e)
      | Let (n, e1, e2) -> Let (n, Self.traverse_expr env e1, Self.traverse_expr env e2)
      | Var n -> Var n
      | IfZero (c, t, f) ->
        IfZero (Self.traverse_expr env c, Self.traverse_expr env t, Self.traverse_expr env f)
      | Call (n, args) -> Call (n, List.map (Self.traverse_expr env) args)

    let traverse_stmt env = function
      | Assign (n, e) -> Assign (n, Self.traverse_expr env e)
      | Seq stmts -> Seq (List.map (Self.traverse_stmt env) stmts)
      | While (e, s) -> While (Self.traverse_expr env e, Self.traverse_stmt env s)
      | Return e -> Return (Self.traverse_expr env e)
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end

(* ================================================================== *)
(* expr / stmt — reduce                                                *)
(* ================================================================== *)

module ExprReduce
    (Env : sig type env end)
    (Mon : sig type s val zero : s val plus : s -> s -> s end)
= struct
  module type S = sig
    val traverse_expr : Env.env -> expr -> Mon.s
    val traverse_stmt : Env.env -> stmt -> Mon.s
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_expr env = function
      | Lit _ -> Mon.zero
      | Add (e1, e2) -> Mon.plus (Self.traverse_expr env e1) (Self.traverse_expr env e2)
      | Neg e -> Self.traverse_expr env e
      | Let (_, e1, e2) -> Mon.plus (Self.traverse_expr env e1) (Self.traverse_expr env e2)
      | Var _ -> Mon.zero
      | IfZero (c, t, f) ->
        Mon.plus (Self.traverse_expr env c)
          (Mon.plus (Self.traverse_expr env t) (Self.traverse_expr env f))
      | Call (_, args) ->
        List.fold_left (fun acc e -> Mon.plus acc (Self.traverse_expr env e)) Mon.zero args

    let traverse_stmt env = function
      | Assign (_, e) -> Self.traverse_expr env e
      | Seq stmts ->
        List.fold_left (fun acc s -> Mon.plus acc (Self.traverse_stmt env s)) Mon.zero stmts
      | While (e, s) -> Mon.plus (Self.traverse_expr env e) (Self.traverse_stmt env s)
      | Return e -> Self.traverse_expr env e
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end

(* ================================================================== *)
(* int tree — iter                                                     *)
(* ================================================================== *)

module TreeIter (Env : sig type env end) = struct
  module type S = sig
    val traverse_tree : Env.env -> int tree -> unit
    val traverse_int : Env.env -> int -> unit
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_tree env = function
      | Leaf -> ()
      | Node (l, v, r) ->
        Self.traverse_tree env l;
        Self.traverse_int env v;
        Self.traverse_tree env r

    let traverse_int _env _x = ()
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end

(* ================================================================== *)
(* int tree — map                                                      *)
(* ================================================================== *)

module TreeMap (Env : sig type env end) = struct
  module type S = sig
    val traverse_tree : Env.env -> int tree -> int tree
    val traverse_int : Env.env -> int -> int
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_tree env = function
      | Leaf -> Leaf
      | Node (l, v, r) ->
        Node (Self.traverse_tree env l, Self.traverse_int env v, Self.traverse_tree env r)

    let traverse_int _env x = x
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end

(* ================================================================== *)
(* int tree — reduce                                                   *)
(* ================================================================== *)

module TreeReduce
    (Env : sig type env end)
    (Mon : sig type s val zero : s val plus : s -> s -> s end)
= struct
  module type S = sig
    val traverse_tree : Env.env -> int tree -> Mon.s
    val traverse_int : Env.env -> int -> Mon.s
  end

  module type V = functor (Self : S) -> S

  module Default (Self : S) : S = struct
    let traverse_tree env = function
      | Leaf -> Mon.zero
      | Node (l, v, r) ->
        Mon.plus (Self.traverse_tree env l)
          (Mon.plus (Self.traverse_int env v) (Self.traverse_tree env r))

    let traverse_int _env _x = Mon.zero
  end

  module Make (V : V) : S = struct
    module rec S : S = V (S)
    include S
  end
end
