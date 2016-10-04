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

(** System interface. *)

val word_size : int
(** Size of one word on the machine currently executing the Caml
    program, in bits: 16. *)

val max_string_length : int
(** Maximum length of a string: 509. *)

val max_array_length : int
(** Maximum length of an array: 255. *)

external sleep : int -> unit = "caml_sleep_millis"
(** sleep n  loops 10000 * n cycles. *)
