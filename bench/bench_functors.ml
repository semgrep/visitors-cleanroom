(* Conformance tests and benchmarks comparing OO visitors (PPX-generated,
   inheriting from VisitorsRuntime) against functor-based visitors on the
   same types. *)

open Types_ours.Test_types
module FV = Functor_visitors

open Bechamel
open Toolkit

(* ================================================================== *)
(* Instantiate functor visitors                                        *)
(* ================================================================== *)

module UnitEnv = struct type env = unit end

(* -- expr iter -- *)
module EI = FV.ExprIter (UnitEnv)
module ExprIterF = EI.Make (EI.Default)

(* -- expr map -- *)
module EM = FV.ExprMap (UnitEnv)
module ExprMapF = EM.Make (EM.Default)

(* -- expr reduce (sum) -- *)
module IntMon = struct type s = int let zero = 0 let plus = ( + ) end
module ER = FV.ExprReduce (UnitEnv) (IntMon)
module ExprReduceF = ER.Make (ER.Default)

(* -- expr iter with override: count literals -- *)
module ExprIterCountLits = EI.Make (functor (Self : EI.S) -> struct
  module Super = EI.Default (Self)
  include Super
  let lit_count = ref 0
  let traverse_expr env e =
    (match e with Lit _ -> incr lit_count | _ -> ());
    Super.traverse_expr env e
end)

(* -- expr reduce with override: collect variables -- *)
module ListMon = struct type s = string list let zero = [] let plus = ( @ ) end
module ERList = FV.ExprReduce (UnitEnv) (ListMon)
module ExprReduceVarsF = ERList.Make (functor (Self : ERList.S) -> struct
  module Super = ERList.Default (Self)
  include Super
  let traverse_expr env = function
    | Var n -> [n]
    | e -> Super.traverse_expr env e
end)

(* -- tree iter -- *)
module TI = FV.TreeIter (UnitEnv)
module TreeIterF = TI.Make (TI.Default)

(* -- tree iter with override: collect ints -- *)
let fn_tree_collected = ref []
module TreeIterCollect = TI.Make (functor (Self : TI.S) -> struct
  module Super = TI.Default (Self)
  include Super
  let traverse_int _env x = fn_tree_collected := x :: !fn_tree_collected
end)

(* -- tree map -- *)
module TM = FV.TreeMap (UnitEnv)
module TreeMapF = TM.Make (TM.Default)

(* -- tree map with override: double ints -- *)
module TreeMapDouble = TM.Make (functor (Self : TM.S) -> struct
  module Super = TM.Default (Self)
  include Super
  let traverse_int _env x = x * 2
end)

(* -- tree reduce (sum) -- *)
module TR = FV.TreeReduce (UnitEnv) (IntMon)
module TreeReduceF = TR.Make (functor (Self : TR.S) -> struct
  module Super = TR.Default (Self)
  include Super
  let traverse_int _env x = x
end)

(* ================================================================== *)
(* OO visitor instances (same traversals)                              *)
(* ================================================================== *)

let oo_expr_iter = object
  inherit [_] expr_iter
end

let oo_expr_map = object
  inherit [_] expr_map
end

let oo_expr_reduce_sum = object
  inherit [_] expr_reduce
  method private zero = 0
  method private plus = ( + )
end

let oo_expr_reduce_vars = object
  inherit [_] expr_reduce
  method private zero = []
  method private plus = ( @ )
  method! visit_Var _env n = [n]
end

let oo_tree_iter = object
  inherit [_] tree_iter
  method visit_'a _env _x = ()
end

let oo_tree_iter_collect collected = object
  inherit [_] tree_iter
  method visit_'a _env x = collected := x :: !collected
end

let oo_tree_map_double = object
  inherit [_] tree_map
  method visit_'a _env x = x * 2
end

let oo_tree_reduce_sum = object
  inherit [_] tree_reduce
  method private zero = 0
  method private plus = ( + )
  method visit_'a _env x = x
end

(* Heavier map function: simulate real work per node *)
let tbl = Hashtbl.create 100
let () = for i = 0 to 99 do Hashtbl.replace tbl (string_of_int i) i done

let oo_expr_map_heavy = object
  inherit [_] expr_map
  method! visit_Lit _env i =
    let s = string_of_int (abs i mod 100) in
    let v = try Hashtbl.find tbl s with Not_found -> i in
    Lit (v + 1)
end

module ExprMapHeavyF = EM.Make (functor (Self : EM.S) -> struct
  module Super = EM.Default (Self)
  include Super
  let traverse_expr env = function
    | Lit i ->
      let s = string_of_int (abs i mod 100) in
      let v = try Hashtbl.find tbl s with Not_found -> i in
      Lit (v + 1)
    | e -> Super.traverse_expr env e
end)

(* ================================================================== *)
(* Test data                                                           *)
(* ================================================================== *)

let rec make_tree depth =
  if depth <= 0 then Leaf
  else Node (make_tree (depth - 1), depth, make_tree (depth - 1))

let make_expr_chain n =
  let rec go i acc =
    if i >= n then acc
    else go (i + 1) (Add (acc, Lit i))
  in go 1 (Lit 0)

let make_expr_deep n =
  (* More realistic: mix of constructors *)
  let rec go i =
    if i <= 0 then Lit 0
    else
      let sub = go (i - 1) in
      match i mod 5 with
      | 0 -> Add (sub, Lit i)
      | 1 -> Neg sub
      | 2 -> Let ("x", Lit i, sub)
      | 3 -> IfZero (Lit 0, sub, Lit i)
      | _ -> Call ("f", [sub; Lit i])
  in go n

let make_stmt_deep n =
  let e = make_expr_deep (n / 2) in
  Seq [
    Assign ("a", e);
    While (Lit 1, Assign ("b", e));
    Return e;
  ]

let tree_12 = make_tree 12
let tree_16 = make_tree 16
let expr_1k = make_expr_chain 1000
let expr_deep = make_expr_deep 500
let stmt_deep = make_stmt_deep 500

(* ================================================================== *)
(* Conformance tests                                                   *)
(* ================================================================== *)

let test_conformance () =
  Printf.printf "--- Conformance: functor vs OO ---\n";
  let pass = ref 0 in
  let fail = ref 0 in
  let check name ok =
    if ok then (incr pass; Printf.printf "  [OK]   %s\n" name)
    else (incr fail; Printf.printf "  [FAIL] %s\n" name)
  in

  (* expr iter: both should produce same side effects *)
  let oo_acc = ref [] in
  let fn_acc = ref [] in
  let oo_iter_acc = object
    inherit [_] expr_iter
    method! visit_int _env x = oo_acc := x :: !oo_acc
  end in
  let module EIAcc = EI.Make (functor (Self : EI.S) -> struct
    module Super = EI.Default (Self)
    include Super
    let traverse_expr env e =
      (match e with Lit i -> fn_acc := i :: !fn_acc | _ -> ());
      Super.traverse_expr env e
  end) in
  oo_iter_acc#visit_expr () expr_1k;
  EIAcc.traverse_expr () expr_1k;
  check "expr iter: visited literals agree" (!oo_acc = !fn_acc);

  (* expr map: negate literals *)
  let oo_map_neg = object
    inherit [_] expr_map
    method! visit_Lit _env i = Lit (-i)
  end in
  let module EMNeg = EM.Make (functor (Self : EM.S) -> struct
    module Super = EM.Default (Self)
    include Super
    let traverse_expr env = function
      | Lit i -> Lit (-i)
      | e -> Super.traverse_expr env e
  end) in
  check "expr map: negate literals agree"
    (oo_map_neg#visit_expr () expr_1k = EMNeg.traverse_expr () expr_1k);

  (* expr map: heavy function *)
  check "expr map heavy: agrees"
    (oo_expr_map_heavy#visit_expr () expr_deep
     = ExprMapHeavyF.traverse_expr () expr_deep);

  (* expr reduce: sum *)
  check "expr reduce: sum agrees"
    (oo_expr_reduce_sum#visit_expr () expr_deep
     = ExprReduceF.traverse_expr () expr_deep);

  (* expr reduce: collect vars *)
  let e_with_vars = Let ("x", Lit 1, Add (Var "x", Var "y")) in
  check "expr reduce: collect vars agrees"
    (oo_expr_reduce_vars#visit_expr () e_with_vars
     = ExprReduceVarsF.traverse_expr () e_with_vars);

  (* tree iter: collect ints *)
  let oo_tree_acc = ref [] in
  let _ = (oo_tree_iter_collect oo_tree_acc)#visit_tree () tree_12 in
  fn_tree_collected := [];
  TreeIterCollect.traverse_tree () tree_12;
  check "tree iter: collected ints agree"
    (!oo_tree_acc = !fn_tree_collected);

  (* tree map: double *)
  let oo_doubled = oo_tree_map_double#visit_tree () tree_12 in
  let fn_doubled = TreeMapDouble.traverse_tree () tree_12 in
  check "tree map: doubled agrees" (oo_doubled = fn_doubled);

  (* tree reduce: sum *)
  check "tree reduce: sum agrees"
    (oo_tree_reduce_sum#visit_tree () tree_12
     = TreeReduceF.traverse_tree () tree_12);

  (* stmt traversals *)
  let oo_stmt_acc = ref [] in
  let fn_stmt_acc = ref [] in
  let oo_stmt_iter = object
    inherit [_] expr_iter
    method! visit_int _env x = oo_stmt_acc := x :: !oo_stmt_acc
  end in
  let module SIAcc = EI.Make (functor (Self : EI.S) -> struct
    module Super = EI.Default (Self)
    include Super
    let traverse_expr env e =
      (match e with Lit i -> fn_stmt_acc := i :: !fn_stmt_acc | _ -> ());
      Super.traverse_expr env e
  end) in
  oo_stmt_iter#visit_stmt () stmt_deep;
  SIAcc.traverse_stmt () stmt_deep;
  check "stmt iter: visited ints agree" (!oo_stmt_acc = !fn_stmt_acc);

  Printf.printf "--- %d passed, %d failed ---\n\n" !pass !fail;
  if !fail > 0 then exit 1

(* ================================================================== *)
(* Allocation measurement                                              *)
(* ================================================================== *)

let measure_alloc name f =
  Gc.full_major ();
  let before = (Gc.stat ()).minor_words in
  f ();
  let after = (Gc.stat ()).minor_words in
  let words = after -. before in
  Printf.printf "  %-50s %10.0f words\n" name words

let test_allocations () =
  Printf.printf "--- Allocation comparison (minor_words) ---\n";

  (* iter: should be zero alloc for both (no values produced) *)
  measure_alloc "expr iter (deep 500) OO"
    (fun () -> oo_expr_iter#visit_expr () expr_deep);
  measure_alloc "expr iter (deep 500) functor"
    (fun () -> ExprIterF.traverse_expr () expr_deep);

  (* map light: both allocate the new AST *)
  measure_alloc "expr map light (deep 500) OO"
    (fun () -> ignore (oo_expr_map#visit_expr () expr_deep));
  measure_alloc "expr map light (deep 500) functor"
    (fun () -> ignore (ExprMapF.traverse_expr () expr_deep));

  (* map heavy *)
  measure_alloc "expr map heavy (deep 500) OO"
    (fun () -> ignore (oo_expr_map_heavy#visit_expr () expr_deep));
  measure_alloc "expr map heavy (deep 500) functor"
    (fun () -> ignore (ExprMapHeavyF.traverse_expr () expr_deep));

  (* reduce: no AST allocation, just monoid values *)
  measure_alloc "expr reduce sum (deep 500) OO"
    (fun () -> ignore (oo_expr_reduce_sum#visit_expr () expr_deep));
  measure_alloc "expr reduce sum (deep 500) functor"
    (fun () -> ignore (ExprReduceF.traverse_expr () expr_deep));

  (* tree iter *)
  measure_alloc "tree iter (depth 12) OO"
    (fun () -> oo_tree_iter#visit_tree () tree_12);
  measure_alloc "tree iter (depth 12) functor"
    (fun () -> TreeIterF.traverse_tree () tree_12);

  (* tree map *)
  measure_alloc "tree map double (depth 12) OO"
    (fun () -> ignore (oo_tree_map_double#visit_tree () tree_12));
  measure_alloc "tree map double (depth 12) functor"
    (fun () -> ignore (TreeMapDouble.traverse_tree () tree_12));

  (* tree reduce *)
  measure_alloc "tree reduce sum (depth 12) OO"
    (fun () -> ignore (oo_tree_reduce_sum#visit_tree () tree_12));
  measure_alloc "tree reduce sum (depth 12) functor"
    (fun () -> ignore (TreeReduceF.traverse_tree () tree_12));

  Printf.printf "---\n\n"

(* ================================================================== *)
(* Benchmarks                                                          *)
(* ================================================================== *)

let benchmarks = Test.make_grouped ~name:"OO vs Functor" [
  Test.make_grouped ~name:"expr iter (1k chain)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      oo_expr_iter#visit_expr () expr_1k));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ExprIterF.traverse_expr () expr_1k));
  ];
  Test.make_grouped ~name:"expr iter (deep 500)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      oo_expr_iter#visit_expr () expr_deep));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ExprIterF.traverse_expr () expr_deep));
  ];
  Test.make_grouped ~name:"expr map light (1k chain)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_expr_map#visit_expr () expr_1k)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (ExprMapF.traverse_expr () expr_1k)));
  ];
  Test.make_grouped ~name:"expr map light (deep 500)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_expr_map#visit_expr () expr_deep)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (ExprMapF.traverse_expr () expr_deep)));
  ];
  Test.make_grouped ~name:"expr map heavy (1k chain)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_expr_map_heavy#visit_expr () expr_1k)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (ExprMapHeavyF.traverse_expr () expr_1k)));
  ];
  Test.make_grouped ~name:"expr map heavy (deep 500)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_expr_map_heavy#visit_expr () expr_deep)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (ExprMapHeavyF.traverse_expr () expr_deep)));
  ];
  Test.make_grouped ~name:"expr reduce sum (deep 500)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_expr_reduce_sum#visit_expr () expr_deep)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (ExprReduceF.traverse_expr () expr_deep)));
  ];
  Test.make_grouped ~name:"stmt iter (deep 500)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      oo_expr_iter#visit_stmt () stmt_deep));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ExprIterF.traverse_stmt () stmt_deep));
  ];
  Test.make_grouped ~name:"tree iter (depth 12)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      oo_tree_iter#visit_tree () tree_12));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      TreeIterF.traverse_tree () tree_12));
  ];
  Test.make_grouped ~name:"tree iter (depth 16)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      oo_tree_iter#visit_tree () tree_16));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      TreeIterF.traverse_tree () tree_16));
  ];
  Test.make_grouped ~name:"tree map double (depth 12)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_tree_map_double#visit_tree () tree_12)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (TreeMapDouble.traverse_tree () tree_12)));
  ];
  Test.make_grouped ~name:"tree reduce sum (depth 12)" [
    Test.make ~name:"OO" (Staged.stage (fun () ->
      ignore (oo_tree_reduce_sum#visit_tree () tree_12)));
    Test.make ~name:"functor" (Staged.stage (fun () ->
      ignore (TreeReduceF.traverse_tree () tree_12)));
  ];
]

(* ================================================================== *)
(* Main                                                                *)
(* ================================================================== *)

let () =
  test_conformance ();
  test_allocations ();
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~limit:2000 ~stabilize:true ~compaction:true
              ~quota:(Time.second 1.) () in
  let raw = Benchmark.all cfg instances benchmarks in
  let results = Analyze.all ols Instance.monotonic_clock raw in
  Printf.printf "--- OO vs Functor benchmarks (ns/run) ---\n";
  Hashtbl.iter (fun test_name result ->
    match Analyze.OLS.estimates result with
    | Some [ ns_per_run ] ->
      Printf.printf "  %-50s %12.1f\n" test_name ns_per_run
    | _ ->
      Printf.printf "  %-50s (no estimate)\n" test_name)
    results;
  Printf.printf "---\n"
