(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

(** Pseudo-random number generators (PRNG). *)

external round : unit -> unit = "caml_random_round"
(** Performs a random round. Usefull for brake deterministic behavior
    of the random algorithm. *)

external bits : unit -> int = "caml_random_bits"
(** Returns 14 random bits in a nonnegative integer. *)

external bool : unit -> bool = "caml_random_bool"
(** Returns [true] or [false] with probability 0.5 each. *)

val int : int -> int
(** [Random.int bound] returns a random integer between 0 (inclusive)
    and [bound] (exclusive).  [bound] must be greater than 0 and less
    than 2{^14}. *)

val int32 : Int32.t -> Int32.t;;
(** [Random.int32 bound] returns a random integer between 0 (inclusive)
    and [bound] (exclusive).  [bound] must be greater than 0. *)

val int64 : Int64.t -> Int64.t;;
(** [Random.int64 bound] returns a random integer between 0 (inclusive)
    and [bound] (exclusive).  [bound] must be greater than 0. *)
