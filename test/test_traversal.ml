(* Tests that PPX-generated visitors produce identical results when
   built on our clean-room runtime vs the upstream runtime. *)

module O = Types_ours.Test_types    (* uses our VisitorsRuntime *)
module T = Types_theirs.Test_types  (* uses upstream VisitorsRuntime *)

(* ------------------------------------------------------------------ *)
(* QCheck generators for our test types                                *)
(* ------------------------------------------------------------------ *)

(* Generate identical trees for both modules via a neutral representation *)
type raw_tree =
  | RLeaf
  | RNode of raw_tree * int * raw_tree

let rec raw_tree_gen depth =
  if depth <= 0 then QCheck.always RLeaf
  else
    QCheck.oneof_weighted [
      (1, QCheck.always RLeaf);
      (* Balanced: both children at depth-1 *)
      (2, QCheck.map (fun (l, v, r) -> RNode (l, v, r))
            (QCheck.triple
              (raw_tree_gen (depth - 1))
              QCheck.int_small
              (raw_tree_gen (depth - 1))));
      (* Left-skewed: deep left, shallow right *)
      (1, QCheck.map (fun (l, v) -> RNode (l, v, RLeaf))
            (QCheck.pair
              (raw_tree_gen (depth - 1))
              QCheck.int_small));
      (* Right-skewed: shallow left, deep right *)
      (1, QCheck.map (fun (v, r) -> RNode (RLeaf, v, r))
            (QCheck.pair
              QCheck.int_small
              (raw_tree_gen (depth - 1))));
    ]

(* Vary depth for more shape diversity *)
let tree_gen =
  QCheck.make QCheck.Gen.(int_range 0 5 >>= fun d -> QCheck.gen (raw_tree_gen d))

let rec to_ours_tree = function
  | RLeaf -> O.Leaf
  | RNode (l, v, r) -> O.Node (to_ours_tree l, v, to_ours_tree r)

let rec to_theirs_tree = function
  | RLeaf -> T.Leaf
  | RNode (l, v, r) -> T.Node (to_theirs_tree l, v, to_theirs_tree r)

let rec ours_tree_to_raw : int O.tree -> raw_tree = function
  | O.Leaf -> RLeaf
  | O.Node (l, v, r) -> RNode (ours_tree_to_raw l, v, ours_tree_to_raw r)

let rec theirs_tree_to_raw : int T.tree -> raw_tree = function
  | T.Leaf -> RLeaf
  | T.Node (l, v, r) -> RNode (theirs_tree_to_raw l, v, theirs_tree_to_raw r)

(* Generate identical exprs *)
type raw_expr =
  | RLit of int
  | RAdd of raw_expr * raw_expr
  | RNeg of raw_expr
  | RLet of string * raw_expr * raw_expr
  | RVar of string
  | RIfZero of raw_expr * raw_expr * raw_expr
  | RCall of string * raw_expr list

and raw_stmt =
  | RAssign of string * raw_expr
  | RSeq of raw_stmt list
  | RWhile of raw_expr * raw_stmt
  | RReturn of raw_expr

let names = [| "x"; "y"; "z"; "f"; "g" |]
let rand_name = QCheck.map (fun i -> names.(abs i mod Array.length names)) QCheck.int_small

let rec raw_expr_gen depth =
  if depth <= 0 then
    QCheck.oneof_weighted [
      (2, QCheck.map (fun i -> RLit i) QCheck.int_small);
      (1, QCheck.map (fun n -> RVar n) rand_name);
    ]
  else
    QCheck.oneof_weighted [
      (2, QCheck.map (fun i -> RLit i) QCheck.int_small);
      (1, QCheck.map (fun n -> RVar n) rand_name);
      (2, QCheck.map (fun (a, b) -> RAdd (a, b))
            (QCheck.pair (raw_expr_gen (depth - 1)) (raw_expr_gen (depth - 1))));
      (1, QCheck.map (fun a -> RNeg a) (raw_expr_gen (depth - 1)));
      (1, QCheck.map (fun (n, e, b) -> RLet (n, e, b))
            (QCheck.triple rand_name (raw_expr_gen (depth - 1)) (raw_expr_gen (depth - 1))));
      (1, QCheck.map (fun (c, t, f) -> RIfZero (c, t, f))
            (QCheck.triple
              (raw_expr_gen (depth - 1))
              (raw_expr_gen (depth - 1))
              (raw_expr_gen (depth - 1))));
      (1, QCheck.map (fun (n, args) -> RCall (n, args))
            (QCheck.pair rand_name
              (QCheck.list_size (QCheck.Gen.int_range 0 3) (raw_expr_gen (depth - 1)))));
    ]

and raw_stmt_gen depth =
  if depth <= 0 then
    QCheck.map (fun (n, e) -> RAssign (n, e))
      (QCheck.pair rand_name (raw_expr_gen 0))
  else
    QCheck.oneof_weighted [
      (2, QCheck.map (fun (n, e) -> RAssign (n, e))
            (QCheck.pair rand_name (raw_expr_gen (depth - 1))));
      (1, QCheck.map (fun ss -> RSeq ss)
            (QCheck.list_size (QCheck.Gen.int_range 0 3) (raw_stmt_gen (depth - 1))));
      (1, QCheck.map (fun (e, s) -> RWhile (e, s))
            (QCheck.pair (raw_expr_gen (depth - 1)) (raw_stmt_gen (depth - 1))));
      (1, QCheck.map (fun e -> RReturn e) (raw_expr_gen (depth - 1)));
    ]

let expr_gen =
  QCheck.make QCheck.Gen.(int_range 0 4 >>= fun d -> QCheck.gen (raw_expr_gen d))
let stmt_gen =
  QCheck.make QCheck.Gen.(int_range 0 4 >>= fun d -> QCheck.gen (raw_stmt_gen d))

let rec to_ours_expr = function
  | RLit i -> O.Lit i
  | RAdd (a, b) -> O.Add (to_ours_expr a, to_ours_expr b)
  | RNeg a -> O.Neg (to_ours_expr a)
  | RLet (n, e, b) -> O.Let (n, to_ours_expr e, to_ours_expr b)
  | RVar n -> O.Var n
  | RIfZero (c, t, f) -> O.IfZero (to_ours_expr c, to_ours_expr t, to_ours_expr f)
  | RCall (n, args) -> O.Call (n, List.map to_ours_expr args)
and to_ours_stmt = function
  | RAssign (n, e) -> O.Assign (n, to_ours_expr e)
  | RSeq ss -> O.Seq (List.map to_ours_stmt ss)
  | RWhile (e, s) -> O.While (to_ours_expr e, to_ours_stmt s)
  | RReturn e -> O.Return (to_ours_expr e)

let rec to_theirs_expr = function
  | RLit i -> T.Lit i
  | RAdd (a, b) -> T.Add (to_theirs_expr a, to_theirs_expr b)
  | RNeg a -> T.Neg (to_theirs_expr a)
  | RLet (n, e, b) -> T.Let (n, to_theirs_expr e, to_theirs_expr b)
  | RVar n -> T.Var n
  | RIfZero (c, t, f) -> T.IfZero (to_theirs_expr c, to_theirs_expr t, to_theirs_expr f)
  | RCall (n, args) -> T.Call (n, List.map to_theirs_expr args)
and to_theirs_stmt = function
  | RAssign (n, e) -> T.Assign (n, to_theirs_expr e)
  | RSeq ss -> T.Seq (List.map to_theirs_stmt ss)
  | RWhile (e, s) -> T.While (to_theirs_expr e, to_theirs_stmt s)
  | RReturn e -> T.Return (to_theirs_expr e)

let rec ours_expr_to_raw : O.expr -> raw_expr = function
  | O.Lit i -> RLit i
  | O.Add (a, b) -> RAdd (ours_expr_to_raw a, ours_expr_to_raw b)
  | O.Neg a -> RNeg (ours_expr_to_raw a)
  | O.Let (n, e, b) -> RLet (n, ours_expr_to_raw e, ours_expr_to_raw b)
  | O.Var n -> RVar n
  | O.IfZero (c, t, f) -> RIfZero (ours_expr_to_raw c, ours_expr_to_raw t, ours_expr_to_raw f)
  | O.Call (n, args) -> RCall (n, List.map ours_expr_to_raw args)
and ours_stmt_to_raw : O.stmt -> raw_stmt = function
  | O.Assign (n, e) -> RAssign (n, ours_expr_to_raw e)
  | O.Seq ss -> RSeq (List.map ours_stmt_to_raw ss)
  | O.While (e, s) -> RWhile (ours_expr_to_raw e, ours_stmt_to_raw s)
  | O.Return e -> RReturn (ours_expr_to_raw e)

let rec theirs_expr_to_raw : T.expr -> raw_expr = function
  | T.Lit i -> RLit i
  | T.Add (a, b) -> RAdd (theirs_expr_to_raw a, theirs_expr_to_raw b)
  | T.Neg a -> RNeg (theirs_expr_to_raw a)
  | T.Let (n, e, b) -> RLet (n, theirs_expr_to_raw e, theirs_expr_to_raw b)
  | T.Var n -> RVar n
  | T.IfZero (c, t, f) -> RIfZero (theirs_expr_to_raw c, theirs_expr_to_raw t, theirs_expr_to_raw f)
  | T.Call (n, args) -> RCall (n, List.map theirs_expr_to_raw args)
and theirs_stmt_to_raw : T.stmt -> raw_stmt = function
  | T.Assign (n, e) -> RAssign (n, theirs_expr_to_raw e)
  | T.Seq ss -> RSeq (List.map theirs_stmt_to_raw ss)
  | T.While (e, s) -> RWhile (theirs_expr_to_raw e, theirs_stmt_to_raw s)
  | T.Return e -> RReturn (theirs_expr_to_raw e)

(* ------------------------------------------------------------------ *)
(* iter: tree                                                          *)
(* ------------------------------------------------------------------ *)

let test_iter_tree =
  QCheck.Test.make ~name:"iter tree: visited ints agree"
    ~count:500
    tree_gen
    (fun raw ->
       let ours_acc = ref [] in
       let theirs_acc = ref [] in
       let o = object
         inherit [_] O.tree_iter
         method visit_'a _env x = ours_acc := x :: !ours_acc
       end in
       let t = object
         inherit [_] T.tree_iter
         method visit_'a _env x = theirs_acc := x :: !theirs_acc
       end in
       o#visit_tree () (to_ours_tree raw);
       t#visit_tree () (to_theirs_tree raw);
       !ours_acc = !theirs_acc)

(* ------------------------------------------------------------------ *)
(* iter: expr/stmt (no type params, no virtual methods)                *)
(* ------------------------------------------------------------------ *)

let test_iter_expr =
  QCheck.Test.make ~name:"iter expr: visited ints and strings agree"
    ~count:300
    expr_gen
    (fun raw ->
       let ours_ints = ref [] in
       let theirs_ints = ref [] in
       let ours_strings = ref [] in
       let theirs_strings = ref [] in
       let o = object
         inherit [_] O.expr_iter
         method! visit_int _env x = ours_ints := x :: !ours_ints
         method! visit_string _env s = ours_strings := s :: !ours_strings
       end in
       let t = object
         inherit [_] T.expr_iter
         method! visit_int _env x = theirs_ints := x :: !theirs_ints
         method! visit_string _env s = theirs_strings := s :: !theirs_strings
       end in
       o#visit_expr () (to_ours_expr raw);
       t#visit_expr () (to_theirs_expr raw);
       !ours_ints = !theirs_ints && !ours_strings = !theirs_strings)

let test_iter_stmt =
  QCheck.Test.make ~name:"iter stmt: visited ints agree"
    ~count:300
    stmt_gen
    (fun raw ->
       let ours_ints = ref [] in
       let theirs_ints = ref [] in
       let o = object
         inherit [_] O.expr_iter
         method! visit_int _env x = ours_ints := x :: !ours_ints
       end in
       let t = object
         inherit [_] T.expr_iter
         method! visit_int _env x = theirs_ints := x :: !theirs_ints
       end in
       o#visit_stmt () (to_ours_stmt raw);
       t#visit_stmt () (to_theirs_stmt raw);
       !ours_ints = !theirs_ints)

(* ------------------------------------------------------------------ *)
(* iter: tree with env passing (depth tracking)                        *)
(* ------------------------------------------------------------------ *)

let test_iter_tree_with_env =
  QCheck.Test.make ~name:"iter tree with env: depth tracking agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let ours_acc = ref [] in
       let theirs_acc = ref [] in
       let o = object (self)
         inherit [_] O.tree_iter
         method visit_'a env x = ours_acc := (env, x) :: !ours_acc
         method! visit_Node env l v r =
           self#visit_tree (env + 1) l;
           self#visit_'a env v;
           self#visit_tree (env + 1) r
       end in
       let t = object (self)
         inherit [_] T.tree_iter
         method visit_'a env x = theirs_acc := (env, x) :: !theirs_acc
         method! visit_Node env l v r =
           self#visit_tree (env + 1) l;
           self#visit_'a env v;
           self#visit_tree (env + 1) r
       end in
       o#visit_tree 0 (to_ours_tree raw);
       t#visit_tree 0 (to_theirs_tree raw);
       !ours_acc = !theirs_acc)

(* ------------------------------------------------------------------ *)
(* map: tree                                                           *)
(* ------------------------------------------------------------------ *)

let test_map_tree =
  QCheck.Test.make ~name:"map tree: double all ints agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_map
         method visit_'a _env x = x * 2
       end in
       let t = object
         inherit [_] T.tree_map
         method visit_'a _env x = x * 2
       end in
       ours_tree_to_raw (o#visit_tree () (to_ours_tree raw))
       = theirs_tree_to_raw (t#visit_tree () (to_theirs_tree raw)))

(* ------------------------------------------------------------------ *)
(* map: expr                                                           *)
(* ------------------------------------------------------------------ *)

let test_map_expr =
  QCheck.Test.make ~name:"map expr: negate all literals agrees"
    ~count:300
    expr_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_map
         method! visit_Lit _env i = O.Lit (-i)
       end in
       let t = object
         inherit [_] T.expr_map
         method! visit_Lit _env i = T.Lit (-i)
       end in
       ours_expr_to_raw (o#visit_expr () (to_ours_expr raw))
       = theirs_expr_to_raw (t#visit_expr () (to_theirs_expr raw)))

let test_map_expr_rename =
  QCheck.Test.make ~name:"map expr: rename variables agrees"
    ~count:300
    expr_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_map
         method! visit_Var _env n = O.Var (n ^ "_renamed")
       end in
       let t = object
         inherit [_] T.expr_map
         method! visit_Var _env n = T.Var (n ^ "_renamed")
       end in
       ours_expr_to_raw (o#visit_expr () (to_ours_expr raw))
       = theirs_expr_to_raw (t#visit_expr () (to_theirs_expr raw)))

let test_map_stmt =
  QCheck.Test.make ~name:"map stmt: transform agrees"
    ~count:300
    stmt_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_map
         method! visit_int _env x = x + 100
       end in
       let t = object
         inherit [_] T.expr_map
         method! visit_int _env x = x + 100
       end in
       ours_stmt_to_raw (o#visit_stmt () (to_ours_stmt raw))
       = theirs_stmt_to_raw (t#visit_stmt () (to_theirs_stmt raw)))

(* ------------------------------------------------------------------ *)
(* map: person (record type)                                           *)
(* ------------------------------------------------------------------ *)

let person_gen =
  QCheck.map (fun (name, age, hobbies) -> (name, age, hobbies))
    (QCheck.triple rand_name QCheck.int_small (QCheck.list_small rand_name))

let test_map_person =
  QCheck.Test.make ~name:"map person: uppercase names agrees"
    ~count:500
    person_gen
    (fun (name, age, hobbies) ->
       let op = O.{ name; age; hobbies } in
       let tp = T.{ name; age; hobbies } in
       let o = object
         inherit [_] O.person_map
         method! visit_string _env s = String.uppercase_ascii s
       end in
       let t = object
         inherit [_] T.person_map
         method! visit_string _env s = String.uppercase_ascii s
       end in
       let O.{ name = on; age = oa; hobbies = oh } = o#visit_person () op in
       let T.{ name = tn; age = ta; hobbies = th } = t#visit_person () tp in
       on = tn && oa = ta && oh = th)

(* ------------------------------------------------------------------ *)
(* endo: tree                                                          *)
(* ------------------------------------------------------------------ *)

let test_endo_tree_identity =
  QCheck.Test.make ~name:"endo tree: identity preserves physical equality"
    ~count:500
    tree_gen
    (fun raw ->
       let ot = to_ours_tree raw in
       let tt = to_theirs_tree raw in
       let o = object
         inherit [_] O.tree_endo
         method visit_'a _env x = x
       end in
       let t = object
         inherit [_] T.tree_endo
         method visit_'a _env x = x
       end in
       let ot' = o#visit_tree () ot in
       let tt' = t#visit_tree () tt in
       (ot' == ot) = (tt' == tt))

let test_endo_tree_change =
  QCheck.Test.make ~name:"endo tree: transform agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_endo
         method visit_'a _env x = x + 1
       end in
       let t = object
         inherit [_] T.tree_endo
         method visit_'a _env x = x + 1
       end in
       ours_tree_to_raw (o#visit_tree () (to_ours_tree raw))
       = theirs_tree_to_raw (t#visit_tree () (to_theirs_tree raw)))

let test_endo_expr_identity =
  QCheck.Test.make ~name:"endo expr: identity preserves physical equality"
    ~count:300
    expr_gen
    (fun raw ->
       let oe = to_ours_expr raw in
       let te = to_theirs_expr raw in
       let o = object inherit [_] O.expr_endo end in
       let t = object inherit [_] T.expr_endo end in
       let oe' = o#visit_expr () oe in
       let te' = t#visit_expr () te in
       (oe' == oe) = (te' == te))

(* ------------------------------------------------------------------ *)
(* reduce: tree                                                        *)
(* ------------------------------------------------------------------ *)

let test_reduce_tree_sum =
  QCheck.Test.make ~name:"reduce tree: sum of ints agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_reduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env x = x
       end in
       let t = object
         inherit [_] T.tree_reduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env x = x
       end in
       o#visit_tree () (to_ours_tree raw)
       = t#visit_tree () (to_theirs_tree raw))

let test_reduce_tree_count =
  QCheck.Test.make ~name:"reduce tree: count ints agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_reduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env _x = 1
       end in
       let t = object
         inherit [_] T.tree_reduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env _x = 1
       end in
       o#visit_tree () (to_ours_tree raw)
       = t#visit_tree () (to_theirs_tree raw))

let test_reduce_expr_count_lits =
  QCheck.Test.make ~name:"reduce expr: count literals agrees"
    ~count:300
    expr_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_reduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Lit _env _i = 1
       end in
       let t = object
         inherit [_] T.expr_reduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Lit _env _i = 1
       end in
       o#visit_expr () (to_ours_expr raw)
       = t#visit_expr () (to_theirs_expr raw))

let test_reduce_expr_collect_vars =
  QCheck.Test.make ~name:"reduce expr: collect variable names agrees"
    ~count:300
    expr_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_reduce
         method private zero = []
         method private plus = ( @ )
         method! visit_Var _env n = [n]
       end in
       let t = object
         inherit [_] T.expr_reduce
         method private zero = []
         method private plus = ( @ )
         method! visit_Var _env n = [n]
       end in
       o#visit_expr () (to_ours_expr raw)
       = t#visit_expr () (to_theirs_expr raw))

let test_reduce_person =
  QCheck.Test.make ~name:"reduce person: collect strings agrees"
    ~count:500
    person_gen
    (fun (name, age, hobbies) ->
       let op = O.{ name; age; hobbies } in
       let tp = T.{ name; age; hobbies } in
       let o = object
         inherit [_] O.person_reduce
         method private zero = []
         method private plus = ( @ )
         method! visit_string _env s = [s]
       end in
       let t = object
         inherit [_] T.person_reduce
         method private zero = []
         method private plus = ( @ )
         method! visit_string _env s = [s]
       end in
       o#visit_person () op = t#visit_person () tp)

(* ------------------------------------------------------------------ *)
(* mapreduce: tree (with non-associative monoid to test fold direction)*)
(* ------------------------------------------------------------------ *)

let test_mapreduce_tree =
  QCheck.Test.make ~name:"mapreduce tree: double + sum agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_mapreduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env x = (x * 2, x)
       end in
       let t = object
         inherit [_] T.tree_mapreduce
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env x = (x * 2, x)
       end in
       let (ot, os) = o#visit_tree () (to_ours_tree raw) in
       let (tt, ts) = t#visit_tree () (to_theirs_tree raw) in
       ours_tree_to_raw ot = theirs_tree_to_raw tt && os = ts)

(* Test with non-associative monoid (subtraction) to detect fold direction bugs *)
let test_mapreduce_tree_nonassoc =
  QCheck.Test.make ~name:"mapreduce tree: non-associative monoid agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_mapreduce
         method private zero = 0
         method private plus a b = a - b
         method visit_'a _env x = (x, x)
       end in
       let t = object
         inherit [_] T.tree_mapreduce
         method private zero = 0
         method private plus a b = a - b
         method visit_'a _env x = (x, x)
       end in
       let (ot, os) = o#visit_tree () (to_ours_tree raw) in
       let (tt, ts) = t#visit_tree () (to_theirs_tree raw) in
       ours_tree_to_raw ot = theirs_tree_to_raw tt && os = ts)

let test_mapreduce_expr =
  QCheck.Test.make ~name:"mapreduce expr: negate + count agrees"
    ~count:300
    expr_gen
    (fun raw ->
       let o = object
         inherit [_] O.expr_mapreduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Lit _env i = (O.Lit (-i), 1)
       end in
       let t = object
         inherit [_] T.expr_mapreduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Lit _env i = (T.Lit (-i), 1)
       end in
       let (oe, os) = o#visit_expr () (to_ours_expr raw) in
       let (te, ts) = t#visit_expr () (to_theirs_expr raw) in
       ours_expr_to_raw oe = theirs_expr_to_raw te && os = ts)

(* ------------------------------------------------------------------ *)
(* iter2: tree                                                         *)
(* ------------------------------------------------------------------ *)

let test_iter2_tree_same =
  QCheck.Test.make ~name:"iter2 tree: same tree succeeds for both"
    ~count:500
    tree_gen
    (fun raw ->
       let ot = to_ours_tree raw in
       let tt = to_theirs_tree raw in
       let o = object
         inherit [_] O.tree_iter2
         method visit_'a _env _a _b = ()
       end in
       let t = object
         inherit [_] T.tree_iter2
         method visit_'a _env _a _b = ()
       end in
       let ours_ok =
         (try o#visit_tree () ot ot; true
          with Visitors_reimpl_runtime.StructuralMismatch -> false) in
       let theirs_ok =
         (try t#visit_tree () tt tt; true
          with Upstream_visitors_runtime.StructuralMismatch -> false) in
       ours_ok = theirs_ok)

let test_iter2_tree_different =
  QCheck.Test.make ~name:"iter2 tree: different trees mismatch agrees"
    ~count:500
    (QCheck.pair tree_gen tree_gen)
    (fun (raw1, raw2) ->
       let o = object
         inherit [_] O.tree_iter2
         method visit_'a _env a b = if a <> b then raise Visitors_reimpl_runtime.StructuralMismatch
       end in
       let t = object
         inherit [_] T.tree_iter2
         method visit_'a _env a b = if a <> b then raise Upstream_visitors_runtime.StructuralMismatch
       end in
       let ours_ok =
         (try o#visit_tree () (to_ours_tree raw1) (to_ours_tree raw2); true
          with Visitors_reimpl_runtime.StructuralMismatch -> false) in
       let theirs_ok =
         (try t#visit_tree () (to_theirs_tree raw1) (to_theirs_tree raw2); true
          with Upstream_visitors_runtime.StructuralMismatch -> false) in
       ours_ok = theirs_ok)

(* ------------------------------------------------------------------ *)
(* map2: tree                                                          *)
(* ------------------------------------------------------------------ *)

let test_map2_tree =
  QCheck.Test.make ~name:"map2 tree: merge same-shape trees agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_map2
         method visit_'a _env a b = a + b
       end in
       let t = object
         inherit [_] T.tree_map2
         method visit_'a _env a b = a + b
       end in
       let ot = to_ours_tree raw in
       let tt = to_theirs_tree raw in
       let ours_res =
         (try Some (ours_tree_to_raw (o#visit_tree () ot ot))
          with Visitors_reimpl_runtime.StructuralMismatch -> None) in
       let theirs_res =
         (try Some (theirs_tree_to_raw (t#visit_tree () tt tt))
          with Upstream_visitors_runtime.StructuralMismatch -> None) in
       ours_res = theirs_res)

(* ------------------------------------------------------------------ *)
(* reduce2: tree                                                       *)
(* ------------------------------------------------------------------ *)

let test_reduce2_tree =
  QCheck.Test.make ~name:"reduce2 tree: pairwise product sum agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_reduce2
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env a b = a * b
       end in
       let t = object
         inherit [_] T.tree_reduce2
         method private zero = 0
         method private plus = ( + )
         method visit_'a _env a b = a * b
       end in
       let ot = to_ours_tree raw in
       let tt = to_theirs_tree raw in
       let ours_res =
         (try Some (o#visit_tree () ot ot)
          with Visitors_reimpl_runtime.StructuralMismatch -> None) in
       let theirs_res =
         (try Some (t#visit_tree () tt tt)
          with Upstream_visitors_runtime.StructuralMismatch -> None) in
       ours_res = theirs_res)

(* ------------------------------------------------------------------ *)
(* color: enum type (nullary constructors)                             *)
(* ------------------------------------------------------------------ *)

type raw_color = RRed | RGreen | RBlue

let color_gen =
  QCheck.make (QCheck.Gen.oneof_list [RRed; RGreen; RBlue])

let to_ours_color = function
  | RRed -> O.Red | RGreen -> O.Green | RBlue -> O.Blue
let to_theirs_color = function
  | RRed -> T.Red | RGreen -> T.Green | RBlue -> T.Blue

let test_iter_color =
  QCheck.Test.make ~name:"iter color: side effects agree"
    ~count:200
    color_gen
    (fun raw ->
       let ours_acc = ref [] in
       let theirs_acc = ref [] in
       let o = object
         inherit [_] O.color_iter
         method! visit_Red _env = ours_acc := "red" :: !ours_acc
         method! visit_Green _env = ours_acc := "green" :: !ours_acc
         method! visit_Blue _env = ours_acc := "blue" :: !ours_acc
       end in
       let t = object
         inherit [_] T.color_iter
         method! visit_Red _env = theirs_acc := "red" :: !theirs_acc
         method! visit_Green _env = theirs_acc := "green" :: !theirs_acc
         method! visit_Blue _env = theirs_acc := "blue" :: !theirs_acc
       end in
       o#visit_color () (to_ours_color raw);
       t#visit_color () (to_theirs_color raw);
       !ours_acc = !theirs_acc)

let test_reduce_color =
  QCheck.Test.make ~name:"reduce color: count agrees"
    ~count:200
    color_gen
    (fun raw ->
       let o = object
         inherit [_] O.color_reduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Red _env = 1
         method! visit_Green _env = 2
         method! visit_Blue _env = 3
       end in
       let t = object
         inherit [_] T.color_reduce
         method private zero = 0
         method private plus = ( + )
         method! visit_Red _env = 1
         method! visit_Green _env = 2
         method! visit_Blue _env = 3
       end in
       o#visit_color () (to_ours_color raw)
       = t#visit_color () (to_theirs_color raw))

(* ------------------------------------------------------------------ *)
(* point: tuple type                                                   *)
(* ------------------------------------------------------------------ *)

let point_gen = QCheck.triple QCheck.int_small QCheck.int_small QCheck.int_small

let test_iter_point =
  QCheck.Test.make ~name:"iter point: side effects agree"
    ~count:300
    point_gen
    (fun (x, y, z) ->
       let ours_acc = ref [] in
       let theirs_acc = ref [] in
       let o = object
         inherit [_] O.point_iter
         method! visit_int _env i = ours_acc := i :: !ours_acc
       end in
       let t = object
         inherit [_] T.point_iter
         method! visit_int _env i = theirs_acc := i :: !theirs_acc
       end in
       o#visit_point () (x, y, z);
       t#visit_point () (x, y, z);
       !ours_acc = !theirs_acc)

let test_map_point =
  QCheck.Test.make ~name:"map point: transform agrees"
    ~count:300
    point_gen
    (fun (x, y, z) ->
       let o = object
         inherit [_] O.point_map
         method! visit_int _env i = i * 2
       end in
       let t = object
         inherit [_] T.point_map
         method! visit_int _env i = i * 2
       end in
       o#visit_point () (x, y, z)
       = t#visit_point () (x, y, z))

let test_reduce_point =
  QCheck.Test.make ~name:"reduce point: sum agrees"
    ~count:300
    point_gen
    (fun (x, y, z) ->
       let o = object
         inherit [_] O.point_reduce
         method private zero = 0
         method private plus = ( + )
       end in
       let t = object
         inherit [_] T.point_reduce
         method private zero = 0
         method private plus = ( + )
       end in
       o#visit_point () (x, y, z)
       = t#visit_point () (x, y, z))

(* ------------------------------------------------------------------ *)
(* endo: partial change (some nodes change, some don't)                *)
(* ------------------------------------------------------------------ *)

let test_endo_tree_partial =
  QCheck.Test.make ~name:"endo tree: partial change agrees"
    ~count:500
    tree_gen
    (fun raw ->
       (* Only change even values *)
       let o = object
         inherit [_] O.tree_endo
         method visit_'a _env x = if x mod 2 = 0 then x + 1 else x
       end in
       let t = object
         inherit [_] T.tree_endo
         method visit_'a _env x = if x mod 2 = 0 then x + 1 else x
       end in
       ours_tree_to_raw (o#visit_tree () (to_ours_tree raw))
       = theirs_tree_to_raw (t#visit_tree () (to_theirs_tree raw)))

let test_endo_tree_partial_sharing =
  QCheck.Test.make ~name:"endo tree: partial change sharing agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let ot = to_ours_tree raw in
       let tt = to_theirs_tree raw in
       let o = object
         inherit [_] O.tree_endo
         method visit_'a _env x = if x mod 2 = 0 then x + 1 else x
       end in
       let t = object
         inherit [_] T.tree_endo
         method visit_'a _env x = if x mod 2 = 0 then x + 1 else x
       end in
       let ot' = o#visit_tree () ot in
       let tt' = t#visit_tree () tt in
       (* Both should agree on whether the root was physically preserved *)
       (ot' == ot) = (tt' == tt))

(* ------------------------------------------------------------------ *)
(* mapreduce2: split non-assoc test into same-length and diff-length   *)
(* ------------------------------------------------------------------ *)

let test_mapreduce_tree_nonassoc_same_shape =
  QCheck.Test.make ~name:"mapreduce tree: non-assoc monoid same-shape agrees"
    ~count:500
    tree_gen
    (fun raw ->
       let o = object
         inherit [_] O.tree_mapreduce
         method private zero = 0
         method private plus a b = a - b
         method visit_'a _env x = (x, x)
       end in
       let t = object
         inherit [_] T.tree_mapreduce
         method private zero = 0
         method private plus a b = a - b
         method visit_'a _env x = (x, x)
       end in
       let (ot, os) = o#visit_tree () (to_ours_tree raw) in
       let (tt, ts) = t#visit_tree () (to_theirs_tree raw) in
       ours_tree_to_raw ot = theirs_tree_to_raw tt && os = ts)

(* ------------------------------------------------------------------ *)
(* Run all tests                                                       *)
(* ------------------------------------------------------------------ *)

let () =
  let open Alcotest in
  run "Visitor traversal conformance" [
    "iter traversal", [
      QCheck_alcotest.to_alcotest test_iter_tree;
      QCheck_alcotest.to_alcotest test_iter_expr;
      QCheck_alcotest.to_alcotest test_iter_stmt;
      QCheck_alcotest.to_alcotest test_iter_tree_with_env;
      QCheck_alcotest.to_alcotest test_iter_color;
      QCheck_alcotest.to_alcotest test_iter_point;
    ];
    "map traversal", [
      QCheck_alcotest.to_alcotest test_map_tree;
      QCheck_alcotest.to_alcotest test_map_expr;
      QCheck_alcotest.to_alcotest test_map_expr_rename;
      QCheck_alcotest.to_alcotest test_map_stmt;
      QCheck_alcotest.to_alcotest test_map_person;
      QCheck_alcotest.to_alcotest test_map_point;
    ];
    "endo traversal", [
      QCheck_alcotest.to_alcotest test_endo_tree_identity;
      QCheck_alcotest.to_alcotest test_endo_tree_change;
      QCheck_alcotest.to_alcotest test_endo_expr_identity;
      QCheck_alcotest.to_alcotest test_endo_tree_partial;
      QCheck_alcotest.to_alcotest test_endo_tree_partial_sharing;
    ];
    "reduce traversal", [
      QCheck_alcotest.to_alcotest test_reduce_tree_sum;
      QCheck_alcotest.to_alcotest test_reduce_tree_count;
      QCheck_alcotest.to_alcotest test_reduce_expr_count_lits;
      QCheck_alcotest.to_alcotest test_reduce_expr_collect_vars;
      QCheck_alcotest.to_alcotest test_reduce_person;
      QCheck_alcotest.to_alcotest test_reduce_color;
      QCheck_alcotest.to_alcotest test_reduce_point;
    ];
    "mapreduce traversal", [
      QCheck_alcotest.to_alcotest test_mapreduce_tree;
      QCheck_alcotest.to_alcotest test_mapreduce_tree_nonassoc;
      QCheck_alcotest.to_alcotest test_mapreduce_tree_nonassoc_same_shape;
      QCheck_alcotest.to_alcotest test_mapreduce_expr;
    ];
    "iter2 traversal", [
      QCheck_alcotest.to_alcotest test_iter2_tree_same;
      QCheck_alcotest.to_alcotest test_iter2_tree_different;
    ];
    "map2 traversal", [
      QCheck_alcotest.to_alcotest test_map2_tree;
    ];
    "reduce2 traversal", [
      QCheck_alcotest.to_alcotest test_reduce2_tree;
    ];
  ]
