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

external run : unit -> unit = "caml_gc_run"
external heap_size : unit -> int = "caml_heap_size"
external stack_size : unit -> int = "caml_stack_size"
external heap_occupation : unit -> int = "caml_heap_occupation"
external stack_occupation : unit -> int = "caml_stack_occupation"
external running_number : unit -> int = "caml_running_number"
