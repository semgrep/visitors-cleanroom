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

(** Clean-room implementation of the VisitorsRuntime module.
    Provides base visitor classes for use with the visitors PPX deriver. *)

(** {1 Utility functions} *)

val array_equal : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** [array_equal eq a1 a2] tests whether [a1] and [a2] have pairwise equal
    elements according to [eq].

    {b Precondition:} The two arrays must have equal length. This function
    asserts equal length rather than returning [false] for mismatched lengths.
    @raise Assert_failure if [Array.length a1 <> Array.length a2]. *)

exception StructuralMismatch
(** Raised by arity-2 visitors when two structures do not have matching shapes. *)

val fail : unit -> 'a
(** [fail ()] raises {!StructuralMismatch}. *)

val wrap : ('a -> 'b) -> 'a -> bool
(** [wrap f x] calls [f x] and returns [true] if it completes normally,
    or [false] if {!StructuralMismatch} is raised. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> bool
(** [wrap2 f x y] calls [f x y] and returns [true] if it completes normally,
    or [false] if {!StructuralMismatch} is raised. *)

(** {1 Monoid classes} *)

(** Virtual base class for monoids.  Inherited by {!reduce},
    {!mapreduce}, {!reduce2}, and {!mapreduce2} to provide the
    [plus] and [zero] methods that those visitors use to combine
    results.  Subclasses must supply concrete implementations.

    The [plus] operation should be associative for predictable
    behavior, though non-associative monoids are permitted (see
    the notes on fold direction in the arity-1 section). *)
class virtual ['s] monoid : object
  method private virtual plus : 's -> 's -> 's
  method private virtual zero : 's
end

(** Concrete monoid over [int] with [plus = (+)] and [zero = 0]. *)
class ['a] addition_monoid : object
  constraint 'a = int
  method private plus : 'a -> 'a -> 'a
  method private zero : 'a
end

(** Concrete monoid over [unit] with [plus = fun () () -> ()] and
    [zero = ()].  Useful for reduce visitors that only care about
    side effects and discard the accumulated value. *)
class ['a] unit_monoid : object
  constraint 'a = unit
  method private plus : 'a -> 'a -> 'a
  method private zero : 'a
end

(** {1 Arity-1 visitor classes}

    {b Implementation requirements for [visit_list]:}

    In [iter], [map], and [endo], the [visit_list] method {e must} be
    implemented via a recursive self-call (i.e. [self#visit_list]) rather
    than delegating to [List.iter], [List.map], etc.  This is because
    PPX-generated subclasses may override [visit_list] and expect each
    recursive step to dispatch through the overridden method. Using a
    standard library function would silently bypass such overrides.

    {b Implementation requirements for [visit_lazy_t] in [endo]:}

    The [endo] visitor's [visit_lazy_t] must {e eagerly} force the lazy
    suspension and compare the result with physical equality ([==]).
    If the transformed value is physically equal to the original, the
    original [Lazy.t] value must be returned to preserve sharing.  A naive
    implementation that wraps the result in [lazy (f env (Lazy.force lz))]
    is incorrect: it defers forcing (changing evaluation semantics) and
    never preserves the original suspension's identity.

    {b Note on [endo#visit_lazy_t] and OCaml 5 domains:}

    Because [visit_lazy_t] eagerly forces the suspension, calling it on
    a [Lazy.t] that is concurrently being forced by another domain will
    raise [Lazy.Undefined].  This is an inherent limitation of the
    eager-force design (upstream has the same behavior).

    {b Implementation requirements for [visit_list] in [mapreduce] and
    [mapreduce2]:}

    The [visit_list] method in [mapreduce] and [mapreduce2] must use a
    {e right fold} (i.e., process the head, then recurse on the tail, then
    combine with [self#plus head_result tail_result]).  A left fold
    reverses the association order of [plus], which produces different
    results when [plus] is not associative.

    {b Note on fold direction for arrays vs. lists in [mapreduce]:}

    [visit_array] uses a {e left fold} (accumulating from index 0 upward),
    while [visit_list] uses a {e right fold} (head then tail).  For
    non-associative monoids, this means the same logical sequence of
    elements produces different accumulation results depending on whether
    it is stored in a list or an array.  This matches upstream behavior.

    {b Note on [visit_float] in arity-2 classes:}

    Arity-2 scalar visitors use structural inequality ([<>]) to detect
    mismatches.  Since [nan <> nan] is [true] under IEEE 754, comparing
    two [nan] values raises {!StructuralMismatch}.  This matches
    upstream. *)

(** Side-effecting traversal.  [visit_list] uses a recursive self-call
    (not [List.iter]) so subclasses can override list traversal behavior. *)
class ['self] iter : object
  method private visit_array :
    'env 'a. ('env -> 'a -> unit) -> 'env -> 'a array -> unit
  method private visit_bool : 'env. 'env -> bool -> unit
  method private visit_bytes : 'env. 'env -> bytes -> unit
  method private visit_char : 'env. 'env -> char -> unit
  method private visit_float : 'env. 'env -> float -> unit
  method private visit_int : 'env. 'env -> int -> unit
  method private visit_int32 : 'env. 'env -> int32 -> unit
  method private visit_int64 : 'env. 'env -> int64 -> unit
  method private visit_lazy_t :
    'env 'a. ('env -> 'a -> unit) -> 'env -> 'a Lazy.t -> unit
  method private visit_list :
    'env 'a. ('env -> 'a -> unit) -> 'env -> 'a list -> unit
  method private visit_nativeint : 'env. 'env -> nativeint -> unit
  method private visit_option :
    'env 'a. ('env -> 'a -> unit) -> 'env -> 'a option -> unit
  method private visit_ref :
    'env 'a. ('env -> 'a -> unit) -> 'env -> 'a ref -> unit
  method private visit_result :
    'env 'a 'e.
      ('env -> 'a -> unit) ->
      ('env -> 'e -> unit) -> 'env -> ('a, 'e) result -> unit
  method private visit_string : 'env. 'env -> string -> unit
  method private visit_unit : 'env. 'env -> unit -> unit
end

(** Structure-transforming traversal (input and output types may differ).
    [visit_list] uses a recursive self-call (not [List.map]) so subclasses
    can override list traversal behavior. *)
class ['self] map : object
  method private visit_array :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array
  method private visit_bool : 'env. 'env -> bool -> bool
  method private visit_bytes : 'env. 'env -> bytes -> bytes
  method private visit_char : 'env. 'env -> char -> char
  method private visit_float : 'env. 'env -> float -> float
  method private visit_int : 'env. 'env -> int -> int
  method private visit_int32 : 'env. 'env -> int32 -> int32
  method private visit_int64 : 'env. 'env -> int64 -> int64
  method private visit_lazy_t :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a Lazy.t -> 'b Lazy.t
  method private visit_list :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list
  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
  method private visit_option :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option
  method private visit_ref :
    'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref
  method private visit_result :
    'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b) ->
      ('env -> 'e -> 'f) ->
      'env -> ('a, 'e) result -> ('b, 'f) result
  method private visit_string : 'env. 'env -> string -> string
  method private visit_unit : 'env. 'env -> unit -> unit
end

(** The [endo] visitor is like {!map} but the input and output types are
    identical, and it preserves physical identity ([==]) when the
    transformation function returns a physically equal value.  This means
    that for container methods ([visit_list], [visit_option], [visit_ref],
    [visit_result], [visit_array], [visit_lazy_t]), the original container
    must be returned unchanged when none of its elements were modified.

    {b [visit_lazy_t]} eagerly forces the suspension, applies [f], and
    returns the original [Lazy.t] if the result is [==] to the forced
    value; otherwise returns [lazy transformed_value].

    {b [visit_list]} uses a recursive self-call (not [List.map]) to enable
    subclass overrides, and returns the original list when no element
    changed. *)
class ['self] endo : object
  method private visit_array :
    'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a array -> 'a array
  method private visit_bool : 'env. 'env -> bool -> bool
  method private visit_bytes : 'env. 'env -> bytes -> bytes
  method private visit_char : 'env. 'env -> char -> char
  method private visit_float : 'env. 'env -> float -> float
  method private visit_int : 'env. 'env -> int -> int
  method private visit_int32 : 'env. 'env -> int32 -> int32
  method private visit_int64 : 'env. 'env -> int64 -> int64
  method private visit_lazy_t :
    'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a Lazy.t -> 'a Lazy.t
  method private visit_list :
    'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a list -> 'a list
  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint
  method private visit_option :
    'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a option -> 'a option
  method private visit_ref :
    'env 'a. ('env -> 'a -> 'a) -> 'env -> 'a ref -> 'a ref
  method private visit_result :
    'env 'a 'e.
      ('env -> 'a -> 'a) ->
      ('env -> 'e -> 'e) ->
      'env -> ('a, 'e) result -> ('a, 'e) result
  method private visit_string : 'env. 'env -> string -> string
  method private visit_unit : 'env. 'env -> unit -> unit
end

(** Traversal that accumulates a monoidal summary.  Subclasses must
    provide [zero] and [plus].

    {b [list_fold_left]} must use a recursive self-call
    ([self#list_fold_left]) rather than [List.fold_left], so that
    subclasses overriding [list_fold_left] see the override at every
    recursive step. *)
class virtual ['self] reduce : object
  method private list_fold_left :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 's -> 'a list -> 's
  method private virtual plus : 's -> 's -> 's
  method private visit_array :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 'a array -> 's
  method private visit_bool : 'env. 'env -> bool -> 's
  method private visit_bytes : 'env. 'env -> bytes -> 's
  method private visit_char : 'env. 'env -> char -> 's
  method private visit_float : 'env. 'env -> float -> 's
  method private visit_int : 'env. 'env -> int -> 's
  method private visit_int32 : 'env. 'env -> int32 -> 's
  method private visit_int64 : 'env. 'env -> int64 -> 's
  method private visit_lazy_t :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 'a Lazy.t -> 's
  method private visit_list :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 'a list -> 's
  method private visit_nativeint : 'env. 'env -> nativeint -> 's
  method private visit_option :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 'a option -> 's
  method private visit_ref :
    'env 'a. ('env -> 'a -> 's) -> 'env -> 'a ref -> 's
  method private visit_result :
    'env 'a 'e.
      ('env -> 'a -> 's) ->
      ('env -> 'e -> 's) -> 'env -> ('a, 'e) result -> 's
  method private visit_string : 'env. 'env -> string -> 's
  method private visit_unit : 'env. 'env -> unit -> 's
  method private virtual zero : 's
end

(** The [mapreduce] visitor simultaneously transforms and accumulates.
    Each [visit_*] method returns a pair [(transformed_value, accumulated_summary)].

    {b [visit_list]} must use a {e right fold}: process the head element,
    then recursively process the tail, then combine with
    [self#plus head_s tail_s].  Using a left fold reverses the association
    order and produces incorrect results when [plus] is non-associative.

    The recursive call goes through [self#visit_list] to allow subclass overrides. *)
class virtual ['self] mapreduce : object
  method private virtual plus : 's -> 's -> 's
  method private visit_array :
    'env 'a 'b.
      ('env -> 'a -> 'b * 's) -> 'env -> 'a array -> 'b array * 's
  method private visit_bool : 'env. 'env -> bool -> bool * 's
  method private visit_bytes : 'env. 'env -> bytes -> bytes * 's
  method private visit_char : 'env. 'env -> char -> char * 's
  method private visit_float : 'env. 'env -> float -> float * 's
  method private visit_int : 'env. 'env -> int -> int * 's
  method private visit_int32 : 'env. 'env -> int32 -> int32 * 's
  method private visit_int64 : 'env. 'env -> int64 -> int64 * 's
  method private visit_lazy_t :
    'env 'a 'b.
      ('env -> 'a -> 'b * 's) -> 'env -> 'a Lazy.t -> 'b Lazy.t * 's
  method private visit_list :
    'env 'a 'b.
      ('env -> 'a -> 'b * 's) -> 'env -> 'a list -> 'b list * 's
  method private visit_nativeint :
    'env. 'env -> nativeint -> nativeint * 's
  method private visit_option :
    'env 'a 'b.
      ('env -> 'a -> 'b * 's) ->
      'env -> 'a option -> 'b option * 's
  method private visit_ref :
    'env 'a 'b.
      ('env -> 'a -> 'b * 's) -> 'env -> 'a ref -> 'b ref * 's
  method private visit_result :
    'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b * 's) ->
      ('env -> 'e -> 'f * 's) ->
      'env ->
      ('a, 'e) result -> ('b, 'f) result * 's
  method private visit_string : 'env. 'env -> string -> string * 's
  method private visit_unit : 'env. 'env -> unit -> unit * 's
  method private virtual zero : 's
end

(** Empty class, a placeholder for user-defined fold visitors.

    No methods are provided because the library does not wish to fix
    their types.  It is up to the user to inherit from a class that
    defines appropriate methods.  {!map} is likely appropriate in many
    situations. *)
class ['self] fold : object end

(** {1 Arity-2 visitor classes}

    The same self-dispatch requirements apply to arity-2 classes:
    [visit_list] in [iter2], [map2], [mapreduce2] must use recursive
    [self#visit_list] calls (not local [let rec] loops) so that
    subclass overrides are respected at each recursive step.

    [mapreduce2#visit_list] must use a right fold, same as
    {!mapreduce}. *)

(** Side-effecting simultaneous traversal of two structures.

    Raises {!StructuralMismatch} when the two structures have
    incompatible shapes (e.g., lists of different lengths, [Some]
    vs. [None], [Ok] vs. [Error]).

    For scalar types ([int], [bool], [float], [string], etc.), the
    two values are compared with [<>] and {!StructuralMismatch} is
    raised if they differ.

    [visit_list] uses a recursive [self#visit_list] call so subclass
    overrides are respected at each recursive step. *)
class ['self] iter2 : object
  method private visit_array :
    'env 'a 'b.
      ('env -> 'a -> 'b -> unit) -> 'env -> 'a array -> 'b array -> unit
  method private visit_bool : 'env. 'env -> bool -> bool -> unit
  method private visit_bytes : 'env. 'env -> bytes -> bytes -> unit
  method private visit_char : 'env. 'env -> char -> char -> unit
  method private visit_float : 'env. 'env -> float -> float -> unit
  method private visit_int : 'env. 'env -> int -> int -> unit
  method private visit_int32 : 'env. 'env -> int32 -> int32 -> unit
  method private visit_int64 : 'env. 'env -> int64 -> int64 -> unit
  method private visit_lazy_t :
    'env 'a 'b.
      ('env -> 'a -> 'b -> unit) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> unit
  method private visit_list :
    'env 'a 'b.
      ('env -> 'a -> 'b -> unit) -> 'env -> 'a list -> 'b list -> unit
  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint -> unit
  method private visit_option :
    'env 'a 'b.
      ('env -> 'a -> 'b -> unit) -> 'env -> 'a option -> 'b option -> unit
  method private visit_ref :
    'env 'a 'b.
      ('env -> 'a -> 'b -> unit) -> 'env -> 'a ref -> 'b ref -> unit
  method private visit_result :
    'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b -> unit) ->
      ('env -> 'e -> 'f -> unit) ->
      'env -> ('a, 'e) result -> ('b, 'f) result -> unit
  method private visit_string : 'env. 'env -> string -> string -> unit
  method private visit_unit : 'env. 'env -> unit -> unit -> unit
end

(** Structure-transforming simultaneous traversal of two structures.

    Raises {!StructuralMismatch} when the two structures have
    incompatible shapes.  For scalar types, the two values are
    compared with [<>] and {!StructuralMismatch} is raised if they
    differ; the first value is returned.

    [visit_list] uses a recursive [self#visit_list] call so subclass
    overrides are respected at each recursive step. *)
class ['self] map2 : object
  method private visit_array :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c) -> 'env -> 'a array -> 'b array -> 'c array
  method private visit_bool : 'env. 'env -> bool -> bool -> bool
  method private visit_bytes : 'env. 'env -> bytes -> bytes -> bytes
  method private visit_char : 'env. 'env -> char -> char -> char
  method private visit_float : 'env. 'env -> float -> float -> float
  method private visit_int : 'env. 'env -> int -> int -> int
  method private visit_int32 : 'env. 'env -> int32 -> int32 -> int32
  method private visit_int64 : 'env. 'env -> int64 -> int64 -> int64
  method private visit_lazy_t :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> 'c Lazy.t
  method private visit_list :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c) -> 'env -> 'a list -> 'b list -> 'c list
  method private visit_nativeint :
    'env. 'env -> nativeint -> nativeint -> nativeint
  method private visit_option :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c) -> 'env -> 'a option -> 'b option -> 'c option
  method private visit_ref :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c) -> 'env -> 'a ref -> 'b ref -> 'c ref
  method private visit_result :
    'env 'a 'b 'c 'e 'f 'g.
      ('env -> 'a -> 'b -> 'c) ->
      ('env -> 'e -> 'f -> 'g) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result -> ('c, 'g) result
  method private visit_string : 'env. 'env -> string -> string -> string
  method private visit_unit : 'env. 'env -> unit -> unit -> unit
end

(** Monoidal reduction over two structures simultaneously.  Subclasses
    must provide [zero] and [plus].

    Raises {!StructuralMismatch} when the two structures have
    incompatible shapes.

    Unlike {!reduce}, [reduce2] does {e not} expose an overridable
    fold helper analogous to [list_fold_left].  The list fold is
    implemented as a local recursive function; subclasses can override
    [visit_list] itself but cannot intercept individual fold steps. *)
class virtual ['self] reduce2 : object
  method private virtual plus : 's -> 's -> 's
  method private visit_array :
    'env 'a 'b.
      ('env -> 'a -> 'b -> 's) -> 'env -> 'a array -> 'b array -> 's
  method private visit_bool : 'env. 'env -> bool -> bool -> 's
  method private visit_bytes : 'env. 'env -> bytes -> bytes -> 's
  method private visit_char : 'env. 'env -> char -> char -> 's
  method private visit_float : 'env. 'env -> float -> float -> 's
  method private visit_int : 'env. 'env -> int -> int -> 's
  method private visit_int32 : 'env. 'env -> int32 -> int32 -> 's
  method private visit_int64 : 'env. 'env -> int64 -> int64 -> 's
  method private visit_lazy_t :
    'env 'a 'b.
      ('env -> 'a -> 'b -> 's) -> 'env -> 'a Lazy.t -> 'b Lazy.t -> 's
  method private visit_list :
    'env 'a 'b.
      ('env -> 'a -> 'b -> 's) -> 'env -> 'a list -> 'b list -> 's
  method private visit_nativeint : 'env. 'env -> nativeint -> nativeint -> 's
  method private visit_option :
    'env 'a 'b.
      ('env -> 'a -> 'b -> 's) -> 'env -> 'a option -> 'b option -> 's
  method private visit_ref :
    'env 'a 'b.
      ('env -> 'a -> 'b -> 's) -> 'env -> 'a ref -> 'b ref -> 's
  method private visit_result :
    'env 'a 'b 'e 'f.
      ('env -> 'a -> 'b -> 's) ->
      ('env -> 'e -> 'f -> 's) ->
      'env -> ('a, 'e) result -> ('b, 'f) result -> 's
  method private visit_string : 'env. 'env -> string -> string -> 's
  method private visit_unit : 'env. 'env -> unit -> unit -> 's
  method private virtual zero : 's
end

(** Simultaneous transform and monoidal accumulation over two
    structures.  Each [visit_*] method returns a pair
    [(transformed_value, accumulated_summary)].  Subclasses must
    provide [zero] and [plus].

    Raises {!StructuralMismatch} when the two structures have
    incompatible shapes.

    [visit_list] uses a {e right fold} via recursive [self#visit_list]
    calls, same as {!mapreduce}. *)
class virtual ['self] mapreduce2 : object
  method private virtual plus : 's -> 's -> 's
  method private visit_array :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c * 's) ->
      'env -> 'a array -> 'b array -> 'c array * 's
  method private visit_bool : 'env. 'env -> bool -> bool -> bool * 's
  method private visit_bytes : 'env. 'env -> bytes -> bytes -> bytes * 's
  method private visit_char : 'env. 'env -> char -> char -> char * 's
  method private visit_float : 'env. 'env -> float -> float -> float * 's
  method private visit_int : 'env. 'env -> int -> int -> int * 's
  method private visit_int32 : 'env. 'env -> int32 -> int32 -> int32 * 's
  method private visit_int64 : 'env. 'env -> int64 -> int64 -> int64 * 's
  method private visit_lazy_t :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c * 's) ->
      'env -> 'a Lazy.t -> 'b Lazy.t -> 'c Lazy.t * 's
  method private visit_list :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c * 's) ->
      'env -> 'a list -> 'b list -> 'c list * 's
  method private visit_nativeint :
    'env. 'env -> nativeint -> nativeint -> nativeint * 's
  method private visit_option :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c * 's) ->
      'env -> 'a option -> 'b option -> 'c option * 's
  method private visit_ref :
    'env 'a 'b 'c.
      ('env -> 'a -> 'b -> 'c * 's) ->
      'env -> 'a ref -> 'b ref -> 'c ref * 's
  method private visit_result :
    'env 'a 'b 'c 'e 'f 'g.
      ('env -> 'a -> 'b -> 'c * 's) ->
      ('env -> 'e -> 'f -> 'g * 's) ->
      'env ->
      ('a, 'e) result ->
      ('b, 'f) result -> ('c, 'g) result * 's
  method private visit_string :
    'env. 'env -> string -> string -> string * 's
  method private visit_unit : 'env. 'env -> unit -> unit -> unit * 's
  method private virtual zero : 's
end

(** Empty class, the arity-2 counterpart of {!fold}.  Same rationale:
    no methods are provided; the user supplies them. *)
class ['self] fold2 : object end
