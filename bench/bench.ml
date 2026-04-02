(* Benchmarks comparing our clean-room VisitorsRuntime against upstream.
   Also includes stack-depth conformance tests. *)

module O = Types_ours.Test_types
module T = Types_theirs.Test_types

open Bechamel
open Toolkit

(* ------------------------------------------------------------------ *)
(* Test data                                                           *)
(* ------------------------------------------------------------------ *)

let rec make_tree depth =
  if depth <= 0 then O.Leaf
  else O.Node (make_tree (depth - 1), depth, make_tree (depth - 1))

let rec make_tree_t depth =
  if depth <= 0 then T.Leaf
  else T.Node (make_tree_t (depth - 1), depth, make_tree_t (depth - 1))

let make_list n = List.init n Fun.id

let make_expr_chain n =
  let rec go i acc =
    if i >= n then acc
    else go (i + 1) (O.Add (acc, O.Lit i))
  in go 1 (O.Lit 0)

let make_expr_chain_t n =
  let rec go i acc =
    if i >= n then acc
    else go (i + 1) (T.Add (acc, T.Lit i))
  in go 1 (T.Lit 0)

(* ------------------------------------------------------------------ *)
(* Visitor instances                                                   *)
(* ------------------------------------------------------------------ *)

let ours_tree_iter = object
  inherit [_] O.tree_iter
  method visit_'a _env _x = ()
end
let theirs_tree_iter = object
  inherit [_] T.tree_iter
  method visit_'a _env _x = ()
end

let ours_tree_map = object
  inherit [_] O.tree_map
  method visit_'a _env x = x + 1
end
let theirs_tree_map = object
  inherit [_] T.tree_map
  method visit_'a _env x = x + 1
end

let ours_tree_endo_id = object
  inherit [_] O.tree_endo
  method visit_'a _env x = x
end
let theirs_tree_endo_id = object
  inherit [_] T.tree_endo
  method visit_'a _env x = x
end

let ours_tree_reduce = object
  inherit [_] O.tree_reduce
  method private zero = 0
  method private plus = ( + )
  method visit_'a _env x = x
end
let theirs_tree_reduce = object
  inherit [_] T.tree_reduce
  method private zero = 0
  method private plus = ( + )
  method visit_'a _env x = x
end

let ours_expr_iter = object inherit [_] O.expr_iter end
let theirs_expr_iter = object inherit [_] T.expr_iter end

class ours_map_list = object
  inherit [_] VisitorsRuntime.map as super
  method list_ (f : unit -> int -> int) xs = super#visit_list f () xs
end
class theirs_map_list = object
  inherit [_] Upstream_visitors_runtime.map as super
  method list_ (f : unit -> int -> int) xs = super#visit_list f () xs
end

(* ------------------------------------------------------------------ *)
(* Test data instances                                                 *)
(* ------------------------------------------------------------------ *)

let tree_12_o = make_tree 12
let tree_12_t = make_tree_t 12
let tree_16_o = make_tree 16
let tree_16_t = make_tree_t 16
let expr_1000_o = make_expr_chain 1000
let expr_1000_t = make_expr_chain_t 1000
let list_10k = make_list 10_000

(* ------------------------------------------------------------------ *)
(* Benchmark definitions                                               *)
(* ------------------------------------------------------------------ *)

let benchmarks = Test.make_grouped ~name:"VisitorsRuntime" [
  Test.make_grouped ~name:"iter tree d12" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ours_tree_iter#visit_tree () tree_12_o));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      theirs_tree_iter#visit_tree () tree_12_t));
  ];
  Test.make_grouped ~name:"iter tree d16" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ours_tree_iter#visit_tree () tree_16_o));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      theirs_tree_iter#visit_tree () tree_16_t));
  ];
  Test.make_grouped ~name:"map tree d12" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ignore (ours_tree_map#visit_tree () tree_12_o)));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      ignore (theirs_tree_map#visit_tree () tree_12_t)));
  ];
  Test.make_grouped ~name:"endo id tree d12" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ignore (ours_tree_endo_id#visit_tree () tree_12_o)));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      ignore (theirs_tree_endo_id#visit_tree () tree_12_t)));
  ];
  Test.make_grouped ~name:"reduce tree d12" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ignore (ours_tree_reduce#visit_tree () tree_12_o)));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      ignore (theirs_tree_reduce#visit_tree () tree_12_t)));
  ];
  Test.make_grouped ~name:"iter expr 1k" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ours_expr_iter#visit_expr () expr_1000_o));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      theirs_expr_iter#visit_expr () expr_1000_t));
  ];
  Test.make_grouped ~name:"map list 10k" [
    Test.make ~name:"ours" (Staged.stage (fun () ->
      ignore ((new ours_map_list)#list_ (fun () x -> x + 1) list_10k)));
    Test.make ~name:"upstream" (Staged.stage (fun () ->
      ignore ((new theirs_map_list)#list_ (fun () x -> x + 1) list_10k)));
  ];
]

(* ------------------------------------------------------------------ *)
(* Stack depth conformance                                             *)
(* ------------------------------------------------------------------ *)

let test_stack_depth () =
  Printf.printf "\n--- Stack depth conformance (8M element list) ---\n";
  let big_list = make_list 8_000_000 in
  let test name ours_fn theirs_fn =
    let ours_ok = (try ours_fn big_list; true with Stack_overflow -> false) in
    let theirs_ok = (try theirs_fn big_list; true with Stack_overflow -> false) in
    let status = if ours_ok = theirs_ok then "MATCH" else "DIFFER" in
    Printf.printf "  [%s] %-25s ours=%-13s upstream=%-13s\n" status name
      (if ours_ok then "ok" else "stackoverflow")
      (if theirs_ok then "ok" else "stackoverflow")
  in
  test "map#visit_list"
    (fun xs -> ignore ((new ours_map_list)#list_ (fun () x -> x) xs))
    (fun xs -> ignore ((new theirs_map_list)#list_ (fun () x -> x) xs));
  let ours_mr = object
    inherit [_] VisitorsRuntime.mapreduce as super
    method private zero = 0
    method private plus = ( + )
    method list_ (f : unit -> int -> int * int) xs = super#visit_list f () xs
  end in
  let theirs_mr = object
    inherit [_] Upstream_visitors_runtime.mapreduce as super
    method private zero = 0
    method private plus = ( + )
    method list_ (f : unit -> int -> int * int) xs = super#visit_list f () xs
  end in
  test "mapreduce#visit_list"
    (fun xs -> ignore (ours_mr#list_ (fun () x -> (x, x)) xs))
    (fun xs -> ignore (theirs_mr#list_ (fun () x -> (x, x)) xs));
  Printf.printf "---\n\n"

(* ------------------------------------------------------------------ *)
(* Main                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  test_stack_depth ();
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~limit:2000 ~stabilize:true ~compaction:true
              ~quota:(Time.second 1.) () in
  let raw = Benchmark.all cfg instances benchmarks in
  let results = Analyze.all ols Instance.monotonic_clock raw in
  let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) results [] in
  let entries = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
  Printf.printf "--- Benchmark results (ns/run) ---\n";
  List.iter (fun (test_name, result) ->
    match Analyze.OLS.estimates result with
    | Some [ ns_per_run ] ->
      Printf.printf "  %-45s %12.1f\n" test_name ns_per_run
    | _ ->
      Printf.printf "  %-45s (no estimate)\n" test_name)
    entries;
  Printf.printf "---\n"
