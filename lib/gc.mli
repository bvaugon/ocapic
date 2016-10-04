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

(** Memory management control and statistics. *)

external run : unit -> unit = "caml_gc_run"
(** Triggers a collection. *)

external heap_size : unit -> int = "caml_heap_size"
(** Returns the total heap size in byte. *)

external stack_size : unit -> int = "caml_stack_size"
(** Returns the total stack level number. *)

external heap_occupation : unit -> int = "caml_heap_occupation"
(** Returns the number of live bytes in the heap. *)

external stack_occupation : unit -> int = "caml_stack_occupation"
(** Returns the current stack size (in word). *)

external running_number : unit -> int = "caml_running_number"
(** Returns number of heap collections since the program was started. *)
