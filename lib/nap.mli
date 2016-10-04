(*************************************************************************)
(*                                                                       *)
(*                           Objective Caml                              *)
(*                                                                       *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                       *)
(*  Copyright 1996 Institut National de Recherche en Informatique et     *)
(*  en Automatique.  All rights reserved.  This file is distributed      *)
(*  under the terms of the GNU Library General Public License, with      *)
(*  the special exception on linking described in file ../OCaml-LICENSE. *)
(*                                                                       *)
(*************************************************************************)

(* $Id: map.mli 10632 2010-07-24 14:16:58Z garrigue $ *)

(** Association tables over ordered types.

    This module implements applicative association tables, also known as
    finite maps or dictionaries, given a total ordering function
    over the keys.
    All operations over maps are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and therefore searching
    and insertion take time logarithmic in the size of the map.
*)

type ('k, 'v) t
  (** The type of maps from type ['k] to type ['v]. *)

val empty: ('k, 'v) t
  (** The empty map. *)

val is_empty: ('k, 'v) t -> bool
  (** Test whether a map is empty or not. *)

val mem: 'k -> ('k, 'v) t -> bool
  (** [mem x m] returns [true] if [m] contains a binding for [x],
      and [false] otherwise. *)

val add: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [add x y m] returns a map containing the same bindings as
      [m], plus a binding of [x] to [y]. If [x] was already bound
      in [m], its previous binding disappears. *)

val singleton: 'k -> 'v -> ('k, 'v) t
  (** [singleton x y] returns the one-element map that contains a binding [y]
      for [x].
      @since 3.12.0
  *)

val remove: 'k -> ('k, 'v) t -> ('k, 'v) t
  (** [remove x m] returns a map containing the same bindings as
      [m], except for [x] which is unbound in the returned map. *)

val merge:
  ('k -> 'v option -> 'w option -> 'x option) -> ('k, 'v) t -> ('k, 'w) t ->
  ('k, 'x) t
  (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
      and of [m2]. The presence of each such binding, and the corresponding
      value, is determined with the function [f].
      @since 3.12.0
  *)

val compare: ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
  (** Total ordering between maps.  The first argument is a total ordering
      used to compare data associated with equal keys in the two maps. *)

val equal: ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool
  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
      equal, that is, contain equal keys and associate them with
      equal data.  [cmp] is the equality predicate used to compare
      the data associated with the keys. *)

val iter: ('k -> 'v -> unit) -> ('k, 'v) t -> unit
  (** [iter f m] applies [f] to all bindings in map [m].
      [f] receives the key as first argument, and the associated value
      as second argument.  The bindings are passed to [f] in increasing
      order with respect to the ordering over the type of the keys. *)

val fold: ('k -> 'v -> 'b -> 'b) -> ('k, 'v) t -> 'b -> 'b
  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order), and [d1 ... dN] are the associated data. *)

val for_all: ('k -> 'v -> bool) -> ('k, 'v) t -> bool
  (** [for_all p m] checks if all the bindings of the map
      satisfy the predicate [p].
      @since 3.12.0
  *)

val exists: ('k -> 'v -> bool) -> ('k, 'v) t -> bool
  (** [exists p m] checks if at least one binding of the map
      satisfy the predicate [p].
      @since 3.12.0
  *)

val filter: ('k -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t
  (** [filter p m] returns the map with all the bindings in [m]
      that satisfy predicate [p].
      @since 3.12.0
  *)

val partition: ('k -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t * ('k, 'v) t
  (** [partition p m] returns a pair of maps [(m1, m2)], where
      [m1] contains all the bindings of [s] that satisfy the
      predicate [p], and [m2] is the map with all the bindings of
      [s] that do not satisfy [p].
      @since 3.12.0
  *)

val cardinal: ('k, 'v) t -> int
  (** Return the number of bindings of a map.
      @since 3.12.0
  *)

val bindings: ('k, 'v) t -> ('k * 'v) list
  (** Return the list of all bindings of the given map.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Map.Make}.
      @since 3.12.0
  *)

val min_binding: ('k, 'v) t -> ('k * 'v)
  (** Return the smallest binding of the given map
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the map is empty.
      @since 3.12.0
  *)

val max_binding: ('k, 'v) t -> ('k * 'v)
  (** Same as {!Map.S.min_binding}, but returns the largest binding
      of the given map.
      @since 3.12.0
  *)

val choose: ('k, 'v) t -> ('k * 'v)
  (** Return one binding of the given map, or raise [Not_found] if
      the map is empty. Which binding is chosen is unspecified,
      but equal bindings will be chosen for equal maps.
      @since 3.12.0
  *)

val split: 'k -> ('k, 'v) t -> ('k, 'v) t * 'v option * ('k, 'v) t
  (** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
      is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
      is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
      @since 3.12.0
  *)

val find: 'k -> ('k, 'v) t -> 'v
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

val map: ('v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  (** [map f m] returns a map with same domain as [m], where the
      associated value [a] of all bindings of [m] has been
      replaced by the result of the application of [f] to [a].
      The bindings are passed to [f] in increasing order
      with respect to the ordering over the type of the keys. *)

val mapi: ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
  (** Same as {!Map.S.map}, but the function receives as arguments both the
      key and the associated value for each binding of the map. *)
