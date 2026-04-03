(* Copyright (C) 2026 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Clean-room implementation of VisitorsRuntime.
   Written from the public API signature only.

   Performance notes:
   - Array methods use manual for-loops with Array.unsafe_get/set to
     avoid closure allocation from partial application of (f env) and
     to eliminate redundant bounds checks where the index is already
     proven in-bounds by the enclosing length check or loop bound.
   - Monoid methods (reduce, mapreduce) cache self#zero and self#plus
     into local let-bindings to hoist virtual dispatch out of loops.
   - endo#visit_array uses a scan-then-copy approach to avoid
     allocating a new array when nothing changes (zero alloc in the
     identity traversal case). *)

(* ---------- Utilities ---------- *)

let array_equal eq a1 a2 =
  let n = Array.length a1 in
  assert (n = Array.length a2);
  let rec loop i =
    (* SAFETY: i is in [0, n) by the loop guard; n = Array.length a1 = Array.length a2 by assert. *)
    i >= n || (eq (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) && loop (i + 1)) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
  in
  loop 0
[@@inline]

exception StructuralMismatch

let fail () = raise StructuralMismatch [@@inline]

let wrap f x =
  match f x with
  | _ -> true
  | exception StructuralMismatch -> false
[@@inline]

let wrap2 f x y =
  match f x y with
  | _ -> true
  | exception StructuralMismatch -> false
[@@inline]

(* ---------- Monoid classes ---------- *)

class virtual ['s] monoid = object
  method private virtual plus : 's -> 's -> 's
  method private virtual zero : 's
end

class ['a] addition_monoid = object
  constraint 'a = int
  method private plus : 'a -> 'a -> 'a = fun a b -> a + b
  method private zero : 'a = 0
end

class ['a] unit_monoid = object
  constraint 'a = unit
  method private plus : 'a -> 'a -> 'a = fun () () -> ()
  method private zero : 'a = ()
end

(* ---------- Arity-1 visitors ---------- *)

class ['self] iter = object (self)
  method private visit_array
    : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
    = fun f env arr ->
      (* Manual loop instead of [Array.iter (f env) arr] to avoid
         allocating a closure for the partial application (f env). *)
      let n = Array.length arr in
      for i = 0 to n - 1 do
        (* SAFETY: i is in [0, n) where n = Array.length arr, by the for-loop bound. *)
        f env (Array.unsafe_get arr i) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
      done

  method private visit_bool : 'env. 'env -> bool -> unit = fun _ _ -> ()
  method private visit_bytes : 'env. 'env -> bytes -> unit = fun _ _ -> ()
  method private visit_char : 'env. 'env -> char -> unit = fun _ _ -> ()
  method private visit_float : 'env. 'env -> float -> unit = fun _ _ -> ()
  method private visit_int : 'env. 'env -> int -> unit = fun _ _ -> ()
  method private visit_int32 : 'env. 'env -> int32 -> unit = fun _ _ -> ()
  method private visit_int64 : 'env. 'env -> int64 -> unit = fun _ _ -> ()

  method private visit_lazy_t
    : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
    = fun f env lz -> f env (Lazy.force lz)

  method private visit_list
    : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
    = fun f env xs ->
      match xs with
      | [] -> ()
      | x :: xs' ->
        f env x;
        (self#visit_list : _ -> _ -> _ list -> _) f env xs'

  method private visit_nativeint : 'env. 'env -> nativeint -> unit = fun _ _ -> ()

  method private visit_option
    : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
    = fun f env -> function None -> () | Some x -> f env x

  method private visit_ref
    : 'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
    = fun f env r -> f env !r

  method private visit_result
    : 'env 'a 'e.
        ('env -> 'a -> unit) ->
        ('env -> 'e -> unit) -> 'env -> ('a, 'e) result -> unit
    = fun f g env -> function Ok x -> f env x | Error e -> g env e

  method private visit_string : 'env. 'env -> string -> unit = fun _ _ -> ()
  method private visit_unit : 'env. 'env -> unit -> unit = fun _ () -> ()
end

class ['self] map = object (self)
  method private visit_array
    : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
    = fun f env arr ->
      (* Manual loop instead of [Array.map (f env) arr] to avoid
         allocating a closure for the partial application (f env).
         We use Array.make seeded with the first element (to get
         the right type for the polymorphic result array), then
         fill the rest. *)
      let n = Array.length arr in
      if n = 0 then [||]
      else begin
        (* SAFETY: index 0 is valid because n > 0 (guarded by if); i in [1, n) by for-loop bound;
           r has length n (Array.make n ...), so unsafe_set r i is in bounds. *)
        let r = Array.make n (f env (Array.unsafe_get arr 0)) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
        for i = 1 to n - 1 do
          Array.unsafe_set r i (f env (Array.unsafe_get arr i)) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
        done;
        r
      end

  method private visit_bool : 'env. 'env -> bool -> bool = fun _ x -> x
  method private visit_bytes : 'env. 'env -> bytes -> bytes = fun _ x -> x
  method private visit_char : 'env. 'env -> char -> char = fun _ x -> x
  method private visit_float : 'env. 'env -> float -> float = fun _ x -> x
  method private visit_int : 'env. 'env -> int -> int = fun _ x -> x
  method private visit_int32 : 'env. 'env -> int32 -> int32 = fun _ x -> x
  method private visit_int64 : 'env. 'env -> int64 -> int64 = fun _ x -> x

  method private visit_lazy_t
    : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
    = fun f env lz -> lazy (f env (Lazy.force lz))

  method private visit_list
    : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
    = fun f env xs ->
      match xs with
      | [] -> []
      | x :: xs' ->
        let y = f env x in
        y :: (self#visit_list : _ -> _ -> _ list -> _) f env xs'

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint = fun _ x -> x

  method private visit_option
    : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
    = fun f env -> function None -> None | Some x -> Some (f env x)

  method private visit_ref
    : 'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
    = fun f env r -> ref (f env !r)

  method private visit_result
    : 'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b) ->
        ('env -> 'e -> 'f) ->
        'env -> ('a, 'e) result -> ('b, 'f) result
    = fun f g env -> function Ok x -> Ok (f env x) | Error e -> Error (g env e)

  method private visit_string : 'env. 'env -> string -> string = fun _ x -> x
  method private visit_unit : 'env. 'env -> unit -> unit = fun _ () -> ()
end

class ['self] endo = object (self)
  method private visit_array
    : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a array -> 'a array
    = fun f env arr ->
      (* Scan-then-copy: instead of always allocating via Array.map
         and discarding the copy when nothing changed, we scan
         elements until the first change.  If no element changes, we
         return the original array with zero allocation.  On first
         change, Array.copy gives us the unchanged prefix cheaply
         (single memcpy), and we fill the rest in-place. *)
      let n = Array.length arr in
      let rec scan i =
        if i >= n then arr  (* nothing changed *)
        else
          (* SAFETY: i is in [0, n) by the scan guard (i >= n returns early).
             arr' has the same length as arr (Array.copy).  j is in [i+1, n)
             by the for-loop bound, and i < n. *)
          let x = Array.unsafe_get arr i in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
          let x' = f env x in
          if x' == x then scan (i + 1)
          else begin
            let arr' = Array.copy arr in
            Array.unsafe_set arr' i x'; (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            for j = i + 1 to n - 1 do
              Array.unsafe_set arr' j (f env (Array.unsafe_get arr j)) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            done;
            arr'
          end
      in
      scan 0

  method private visit_bool : 'env. 'env -> bool -> bool = fun _ x -> x
  method private visit_bytes : 'env. 'env -> bytes -> bytes = fun _ x -> x
  method private visit_char : 'env. 'env -> char -> char = fun _ x -> x
  method private visit_float : 'env. 'env -> float -> float = fun _ x -> x
  method private visit_int : 'env. 'env -> int -> int = fun _ x -> x
  method private visit_int32 : 'env. 'env -> int32 -> int32 = fun _ x -> x
  method private visit_int64 : 'env. 'env -> int64 -> int64 = fun _ x -> x

  method private visit_lazy_t
    : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a Lazy.t -> 'a Lazy.t
    = fun f env lz ->
      let x = Lazy.force lz in
      let x' = f env x in
      if x == x' then lz else lazy x'

  method private visit_list
    : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a list -> 'a list
    = fun f env xs ->
      match xs with
      | [] -> xs
      | x :: xs' ->
        let x' = f env x in
        let xs'' = (self#visit_list : _ -> _ -> _ list -> _) f env xs' in
        if x == x' && xs' == xs'' then xs else x' :: xs''

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint = fun _ x -> x

  method private visit_option
    : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a option -> 'a option
    = fun f env -> function
      | None -> None
      | Some x as orig ->
        let x' = f env x in
        if x' == x then orig else Some x'

  method private visit_ref
    : 'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a ref -> 'a ref
    = fun f env r ->
      let v = !r in
      let v' = f env v in
      if v' == v then r else ref v'

  method private visit_result
    : 'env 'a 'e.
        ('env -> 'a -> 'a) ->
        ('env -> 'e -> 'e) ->
        'env -> ('a, 'e) result -> ('a, 'e) result
    = fun f g env res ->
      match res with
      | Ok x ->
        let x' = f env x in
        if x' == x then res else Ok x'
      | Error e ->
        let e' = g env e in
        if e' == e then res else Error e'

  method private visit_string : 'env. 'env -> string -> string = fun _ x -> x
  method private visit_unit : 'env. 'env -> unit -> unit = fun _ () -> ()
end

class virtual ['self] reduce = object (self)
  inherit ['s] monoid

  method private list_fold_left
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 's -> 'a list -> 's
    = fun f env s xs ->
      match xs with
      | [] -> s
      | x :: xs' ->
        let s = self#plus s (f env x) in
        (self#list_fold_left : _ -> _ -> _ -> _ list -> _) f env s xs'

  method private visit_array
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 'a array -> 's
    = fun f env arr ->
      (* Manual loop instead of [Array.fold_left (fun acc x ->
         self#plus acc (f env x)) self#zero arr] to avoid allocating
         the closure that would capture self, f, and env.  We also
         cache self#plus into a local binding to hoist the virtual
         dispatch out of the loop. *)
      let n = Array.length arr in
      let plus = self#plus in
      let rec loop acc i =
        if i >= n then acc
        (* SAFETY: i is in [0, n) by the loop guard; n = Array.length arr. *)
        else loop (plus acc (f env (Array.unsafe_get arr i))) (i + 1) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
      in
      loop self#zero 0

  method private visit_bool : 'env. 'env -> bool -> 's = fun _ _ -> self#zero
  method private visit_bytes : 'env. 'env -> bytes -> 's = fun _ _ -> self#zero
  method private visit_char : 'env. 'env -> char -> 's = fun _ _ -> self#zero
  method private visit_float : 'env. 'env -> float -> 's = fun _ _ -> self#zero
  method private visit_int : 'env. 'env -> int -> 's = fun _ _ -> self#zero
  method private visit_int32 : 'env. 'env -> int32 -> 's = fun _ _ -> self#zero
  method private visit_int64 : 'env. 'env -> int64 -> 's = fun _ _ -> self#zero

  method private visit_lazy_t
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 'a Lazy.t -> 's
    = fun f env lz -> f env (Lazy.force lz)

  method private visit_list
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 'a list -> 's
    = fun f env xs ->
      self#list_fold_left f env self#zero xs

  method private visit_nativeint : 'env. 'env -> nativeint -> 's = fun _ _ -> self#zero

  method private visit_option
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 'a option -> 's
    = fun f env -> function None -> self#zero | Some x -> f env x

  method private visit_ref
    : 'env 'a. ('env -> 'a -> 's) -> 'env -> 'a ref -> 's
    = fun f env r -> f env !r

  method private visit_result
    : 'env 'a 'e.
        ('env -> 'a -> 's) ->
        ('env -> 'e -> 's) -> 'env -> ('a, 'e) result -> 's
    = fun f g env -> function Ok x -> f env x | Error e -> g env e

  method private visit_string : 'env. 'env -> string -> 's = fun _ _ -> self#zero
  method private visit_unit : 'env. 'env -> unit -> 's = fun _ () -> self#zero
end

class virtual ['self] mapreduce = object (self)
  inherit ['s] monoid

  method private visit_array
    : 'env 'a 'b.
        ('env -> 'a -> 'b * 's) -> 'env -> 'a array -> 'b array * 's
    = fun f env arr ->
      (* Manual loop instead of Array.map with a ref accumulator.
         Caches self#plus and seeds Array.make with the first element
         to avoid both the closure allocation and the ref boxing
         overhead of the naive approach. *)
      let n = Array.length arr in
      let plus = self#plus in
      if n = 0 then ([||], self#zero)
      else begin
        (* SAFETY: index 0 is valid because n > 0; i in [1, n) by loop guard;
           r has length n (Array.make n ...). *)
        let (y0, s0) = f env (Array.unsafe_get arr 0) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
        let r = Array.make n y0 in
        let rec loop acc i =
          if i >= n then (r, acc)
          else
            let (y, s) = f env (Array.unsafe_get arr i) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            Array.unsafe_set r i y; (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            loop (plus acc s) (i + 1)
        in
        loop (plus self#zero s0) 1
      end

  method private visit_bool : 'env. 'env -> bool -> bool * 's
    = fun _ x -> (x, self#zero)

  method private visit_bytes : 'env. 'env -> bytes -> bytes * 's
    = fun _ x -> (x, self#zero)

  method private visit_char : 'env. 'env -> char -> char * 's
    = fun _ x -> (x, self#zero)

  method private visit_float : 'env. 'env -> float -> float * 's
    = fun _ x -> (x, self#zero)

  method private visit_int : 'env. 'env -> int -> int * 's
    = fun _ x -> (x, self#zero)

  method private visit_int32 : 'env. 'env -> int32 -> int32 * 's
    = fun _ x -> (x, self#zero)

  method private visit_int64 : 'env. 'env -> int64 -> int64 * 's
    = fun _ x -> (x, self#zero)

  method private visit_lazy_t
    : 'env 'a 'b.
        ('env -> 'a -> 'b * 's) -> 'env -> 'a Lazy.t -> 'b Lazy.t * 's
    = fun f env lz ->
      let (y, s) = f env (Lazy.force lz) in
      (lazy y, s)

  method private visit_list
    : 'env 'a 'b.
        ('env -> 'a -> 'b * 's) -> 'env -> 'a list -> 'b list * 's
    = fun f env xs ->
      match xs with
      | [] -> ([], self#zero)
      | x :: xs' ->
        let (y, sy) = f env x in
        let (ys, sys) = (self#visit_list : _ -> _ -> _ list -> _) f env xs' in
        (y :: ys, self#plus sy sys)

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint * 's
    = fun _ x -> (x, self#zero)

  method private visit_option
    : 'env 'a 'b.
        ('env -> 'a -> 'b * 's) -> 'env -> 'a option -> 'b option * 's
    = fun f env -> function
      | None -> (None, self#zero)
      | Some x ->
        let (y, s) = f env x in
        (Some y, s)

  method private visit_ref
    : 'env 'a 'b.
        ('env -> 'a -> 'b * 's) -> 'env -> 'a ref -> 'b ref * 's
    = fun f env r ->
      let (y, s) = f env !r in
      (ref y, s)

  method private visit_result
    : 'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b * 's) ->
        ('env -> 'e -> 'f * 's) ->
        'env -> ('a, 'e) result -> ('b, 'f) result * 's
    = fun f g env -> function
      | Ok x ->
        let (y, s) = f env x in
        (Ok y, s)
      | Error e ->
        let (e', s) = g env e in
        (Error e', s)

  method private visit_string : 'env. 'env -> string -> string * 's
    = fun _ x -> (x, self#zero)

  method private visit_unit : 'env. 'env -> unit -> unit * 's
    = fun _ () -> ((), self#zero)
end

class ['self] fold = object end

(* ---------- Arity-2 visitors ---------- *)

(* Arity-2 array helpers use the same manual-loop / unsafe_get pattern
   as the arity-1 methods above, for the same reasons: avoid closure
   allocation from partial application and eliminate redundant bounds
   checks. Length is validated before the loop, so all indices are
   proven in-bounds. *)

let iter2_array f env a1 a2 =
  let n = Array.length a1 in
  if n <> Array.length a2 then raise StructuralMismatch;
  for i = 0 to n - 1 do
    (* SAFETY: i is in [0, n) where n = Array.length a1 = Array.length a2, by the for-loop bound and length check. *)
    f env (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
  done

let map2_array f env a1 a2 =
  let n = Array.length a1 in
  if n <> Array.length a2 then raise StructuralMismatch;
  if n = 0 then [||]
  else begin
    (* SAFETY: index 0 is valid because n > 0; i in [1, n) by for-loop bound;
       n = Array.length a1 = Array.length a2 by the length check above;
       r has length n (Array.make n ...), so unsafe_set r i is in bounds. *)
    let r = Array.make n (f env (Array.unsafe_get a1 0) (Array.unsafe_get a2 0)) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
    for i = 1 to n - 1 do
      Array.unsafe_set r i (f env (Array.unsafe_get a1 i) (Array.unsafe_get a2 i)) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
    done;
    r
  end

class ['self] iter2 = object (self)
  method private visit_array
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array -> unit
    = fun f env a1 a2 -> iter2_array f env a1 a2

  method private visit_bool : 'env. 'env -> bool -> bool -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_bytes : 'env. 'env -> bytes -> bytes -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_char : 'env. 'env -> char -> char -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_float : 'env. 'env -> float -> float -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_int : 'env. 'env -> int -> int -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_int32 : 'env. 'env -> int32 -> int32 -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_int64 : 'env. 'env -> int64 -> int64 -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_lazy_t
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> unit) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> unit
    = fun f env lz1 lz2 -> f env (Lazy.force lz1) (Lazy.force lz2)

  method private visit_list
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list -> unit
    = fun f env xs ys ->
      match xs, ys with
      | [], [] -> ()
      | x :: xs', y :: ys' ->
        f env x y;
        (self#visit_list : _ -> _ -> _ list -> _ list -> _) f env xs' ys'
      | _ -> raise StructuralMismatch

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_option
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option -> unit
    = fun f env a b ->
      match a, b with
      | None, None -> ()
      | Some x, Some y -> f env x y
      | _ -> raise StructuralMismatch

  method private visit_ref
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> unit) -> 'env -> 'a ref -> 'b ref -> unit
    = fun f env r1 r2 -> f env !r1 !r2

  method private visit_result
    : 'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b -> unit) ->
        ('env -> 'e -> 'f -> unit) ->
        'env -> ('a, 'e) result -> ('b, 'f) result -> unit
    = fun f g env a b ->
      match a, b with
      | Ok x, Ok y -> f env x y
      | Error x, Error y -> g env x y
      | _ -> raise StructuralMismatch

  method private visit_string : 'env. 'env -> string -> string -> unit
    = fun _ a b -> if a <> b then raise StructuralMismatch

  method private visit_unit : 'env. 'env -> unit -> unit -> unit
    = fun _ () () -> ()
end

class ['self] map2 = object (self)
  method private visit_array
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c) -> 'env -> 'a array -> 'b array -> 'c array
    = fun f env a1 a2 -> map2_array f env a1 a2

  method private visit_bool : 'env. 'env -> bool -> bool -> bool
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_bytes : 'env. 'env -> bytes -> bytes -> bytes
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_char : 'env. 'env -> char -> char -> char
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_float : 'env. 'env -> float -> float -> float
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_int : 'env. 'env -> int -> int -> int
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_int32 : 'env. 'env -> int32 -> int32 -> int32
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_int64 : 'env. 'env -> int64 -> int64 -> int64
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_lazy_t
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> 'c Lazy.t
    = fun f env lz1 lz2 -> lazy (f env (Lazy.force lz1) (Lazy.force lz2))

  method private visit_list
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c) -> 'env -> 'a list -> 'b list -> 'c list
    = fun f env xs ys ->
      match xs, ys with
      | [], [] -> []
      | x :: xs', y :: ys' ->
        let z = f env x y in
        z :: (self#visit_list : _ -> _ -> _ list -> _ list -> _) f env xs' ys'
      | _ -> raise StructuralMismatch

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint -> nativeint
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_option
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c) -> 'env -> 'a option -> 'b option -> 'c option
    = fun f env a b ->
      match a, b with
      | None, None -> None
      | Some x, Some y -> Some (f env x y)
      | _ -> raise StructuralMismatch

  method private visit_ref
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c) -> 'env -> 'a ref -> 'b ref -> 'c ref
    = fun f env r1 r2 -> ref (f env !r1 !r2)

  method private visit_result
    : 'env 'a 'b 'c 'e 'f 'g.
        ('env -> 'a -> 'b -> 'c) ->
        ('env -> 'e -> 'f -> 'g) ->
        'env ->
        ('a, 'e) result ->
        ('b, 'f) result -> ('c, 'g) result
    = fun f g env a b ->
      match a, b with
      | Ok x, Ok y -> Ok (f env x y)
      | Error x, Error y -> Error (g env x y)
      | _ -> raise StructuralMismatch

  method private visit_string : 'env. 'env -> string -> string -> string
    = fun _ a b -> if a <> b then raise StructuralMismatch; a

  method private visit_unit : 'env. 'env -> unit -> unit -> unit
    = fun _ () () -> ()
end

class virtual ['self] reduce2 = object (self)
  inherit ['s] monoid

  method private visit_array
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> 's) -> 'env -> 'a array -> 'b array -> 's
    = fun f env a1 a2 ->
      (* Same manual-loop + cached plus pattern as reduce#visit_array. *)
      let n = Array.length a1 in
      if n <> Array.length a2 then raise StructuralMismatch;
      let plus = self#plus in
      let rec loop acc i =
        if i >= n then acc
        (* SAFETY: i is in [0, n) by the loop guard; n = Array.length a1 = Array.length a2 by the length check. *)
        else loop (plus acc (f env (Array.unsafe_get a1 i) (Array.unsafe_get a2 i))) (i + 1) (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
      in
      loop self#zero 0

  method private visit_bool : 'env. 'env -> bool -> bool -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_bytes : 'env. 'env -> bytes -> bytes -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_char : 'env. 'env -> char -> char -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_float : 'env. 'env -> float -> float -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_int : 'env. 'env -> int -> int -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_int32 : 'env. 'env -> int32 -> int32 -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_int64 : 'env. 'env -> int64 -> int64 -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_lazy_t
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> 's) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> 's
    = fun f env lz1 lz2 -> f env (Lazy.force lz1) (Lazy.force lz2)

  method private visit_list
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> 's) -> 'env -> 'a list -> 'b list -> 's
    = fun f env xs ys ->
      match xs, ys with
      | [], [] -> self#zero
      | x :: xs', y :: ys' ->
        let s = f env x y in
        self#plus s
          ((self#visit_list : _ -> _ -> _ list -> _ list -> _) f env xs' ys')
      | _ -> raise StructuralMismatch

  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_option
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> 's) -> 'env -> 'a option -> 'b option -> 's
    = fun f env a b ->
      match a, b with
      | None, None -> self#zero
      | Some x, Some y -> f env x y
      | _ -> raise StructuralMismatch

  method private visit_ref
    : 'env 'a 'b.
        ('env -> 'a -> 'b -> 's) -> 'env -> 'a ref -> 'b ref -> 's
    = fun f env r1 r2 -> f env !r1 !r2

  method private visit_result
    : 'env 'a 'b 'e 'f.
        ('env -> 'a -> 'b -> 's) ->
        ('env -> 'e -> 'f -> 's) ->
        'env -> ('a, 'e) result -> ('b, 'f) result -> 's
    = fun f g env a b ->
      match a, b with
      | Ok x, Ok y -> f env x y
      | Error x, Error y -> g env x y
      | _ -> raise StructuralMismatch

  method private visit_string : 'env. 'env -> string -> string -> 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; self#zero

  method private visit_unit : 'env. 'env -> unit -> unit -> 's
    = fun _ () () -> self#zero
end

class virtual ['self] mapreduce2 = object (self)
  inherit ['s] monoid

  method private visit_array
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c * 's) ->
        'env -> 'a array -> 'b array -> 'c array * 's
    = fun f env a1 a2 ->
      (* Same manual-loop + cached plus pattern as mapreduce#visit_array. *)
      let n = Array.length a1 in
      if n <> Array.length a2 then raise StructuralMismatch;
      let plus = self#plus in
      if n = 0 then ([||], self#zero)
      else begin
        (* SAFETY: index 0 is valid because n > 0; i in [1, n) by loop guard;
           n = Array.length a1 = Array.length a2 by the length check; r has length n. *)
        let (z0, s0) = f env (Array.unsafe_get a1 0) (Array.unsafe_get a2 0) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
        let r = Array.make n z0 in
        let rec loop acc i =
          if i >= n then (r, acc)
          else
            let (z, s) = f env (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) in (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            Array.unsafe_set r i z; (* nosemgrep: ocaml.lang.security.unsafe.ocamllint-unsafe *)
            loop (plus acc s) (i + 1)
        in
        loop s0 1
      end

  method private visit_bool : 'env. 'env -> bool -> bool -> bool * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_bytes : 'env. 'env -> bytes -> bytes -> bytes * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_char : 'env. 'env -> char -> char -> char * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_float : 'env. 'env -> float -> float -> float * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_int : 'env. 'env -> int -> int -> int * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_int32 : 'env. 'env -> int32 -> int32 -> int32 * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_int64 : 'env. 'env -> int64 -> int64 -> int64 * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_lazy_t
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c * 's) ->
        'env -> 'a Lazy.t -> 'b Lazy.t -> 'c Lazy.t * 's
    = fun f env lz1 lz2 ->
      let (z, s) = f env (Lazy.force lz1) (Lazy.force lz2) in
      (lazy z, s)

  method private visit_list
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c * 's) ->
        'env -> 'a list -> 'b list -> 'c list * 's
    = fun f env xs ys ->
      match xs, ys with
      | [], [] -> ([], self#zero)
      | x :: xs', y :: ys' ->
        let (z, sz) = f env x y in
        let (zs, szs) = (self#visit_list : _ -> _ -> _ list -> _ list -> _) f env xs' ys' in
        (z :: zs, self#plus sz szs)
      | _ -> raise StructuralMismatch

  method private visit_nativeint
    : 'env. 'env -> nativeint -> nativeint -> nativeint * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_option
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c * 's) ->
        'env -> 'a option -> 'b option -> 'c option * 's
    = fun f env a b ->
      match a, b with
      | None, None -> (None, self#zero)
      | Some x, Some y ->
        let (z, s) = f env x y in
        (Some z, s)
      | _ -> raise StructuralMismatch

  method private visit_ref
    : 'env 'a 'b 'c.
        ('env -> 'a -> 'b -> 'c * 's) ->
        'env -> 'a ref -> 'b ref -> 'c ref * 's
    = fun f env r1 r2 ->
      let (z, s) = f env !r1 !r2 in
      (ref z, s)

  method private visit_result
    : 'env 'a 'b 'c 'e 'f 'g.
        ('env -> 'a -> 'b -> 'c * 's) ->
        ('env -> 'e -> 'f -> 'g * 's) ->
        'env ->
        ('a, 'e) result ->
        ('b, 'f) result -> ('c, 'g) result * 's
    = fun f g env a b ->
      match a, b with
      | Ok x, Ok y ->
        let (z, s) = f env x y in
        (Ok z, s)
      | Error x, Error y ->
        let (z, s) = g env x y in
        (Error z, s)
      | _ -> raise StructuralMismatch

  method private visit_string : 'env. 'env -> string -> string -> string * 's
    = fun _ a b -> if a <> b then raise StructuralMismatch; (a, self#zero)

  method private visit_unit : 'env. 'env -> unit -> unit -> unit * 's
    = fun _ () () -> ((), self#zero)
end

class ['self] fold2 = object end
